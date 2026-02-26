{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.chvp.base.zfs.enable {
    chvp.dataPrefix = lib.mkDefault "/data";
    chvp.cachePrefix = lib.mkDefault "/cache";

    boot = {
      supportedFilesystems = [ "zfs" ];
      zfs.requestEncryptionCredentials = config.chvp.base.zfs.encrypted;
      initrd.systemd = {
        enable = true;
        services.rollback = {
          description = "Rollback root filesystem to a pristine state on boot";
          wantedBy = [ "initrd.target" ];
          after = [ "zfs-import-${config.chvp.base.zfs.rootPool}.service" ];
          before = [ "sysroot.mount" ];
          path = with pkgs; [ zfs ];
          unitConfig.DefaultDependencies = "no";
          serviceConfig.Type = "oneshot";
          script = ''
            ${config.boot.zfs.package}/bin/zfs rollback -r ${config.chvp.base.zfs.rootDataset}@blank && echo "  >> >> rollback complete << <<"
          '';
        };
      };
    };

    services = {
      znapzend = {
        enable = config.chvp.base.zfs.backups != [ ];
        pure = true;
        autoCreation = true;
        zetup = builtins.listToAttrs
          (map
            (elem: {
              name = elem.path;
              value = {
                enable = true;
                plan =
                  if elem.fast then
                    "1hour=>15min,1day=>1hour,1week=>1day,4week=>1week" else
                    "1day=>1hour,1week=>1day,4week=>1week,1year=>1month,10year=>6month";
                timestampFormat = "%Y-%m-%d--%H%M%SZ";
                destinations."${elem.location}" = {
                  plan =
                    if elem.fast then
                      "1day=>1hour,1week=>1day,4week=>1week,1year=>4week,10year=>1year" else
                      "1day=>1hour,1week=>1day,4week=>1week,1year=>1month,10year=>6month";
                  host = "${elem.location}";
                  dataset = elem.remotePath;
                };
              };
            })
            config.chvp.base.zfs.backups);

      };
      zfs = {
        autoScrub.enable = true;
        trim.enable = true;
      };
    };

    system.activationScripts =
      let
        ensureSystemExistsScript = lib.concatStringsSep "\n" (map (path: ''mkdir -p "${path}"'') config.chvp.base.zfs.ensureSystemExists);
        ensureHomeExistsScript = lib.concatStringsSep "\n" (map (path: ''mkdir -p "/home/charlotte/${path}"'') config.chvp.base.zfs.ensureHomeExists);
        ensureHomePermissionsScript = lib.concatStringsSep "\n" (map (path: ''chown charlotte:users /home/charlotte/${path}'') config.chvp.base.zfs.ensureHomeExists);
      in
      {
        ensureSystemPathsExist = {
          text = ensureSystemExistsScript;
          deps = [ "agenixNewGeneration" ];
        };
        ensureHomePathsExist = {
          text = ''
            mkdir -p /home/charlotte/
            ${ensureHomeExistsScript}
          '';
        };
        agenixInstall.deps = [ "ensureSystemPathsExist" "ensureHomePathsExist" ];
        ensureHomePermissionsScript = {
          text = ''
            chown charlotte:users /home/charlotte
            ${ensureHomePermissionsScript}
          '';
          deps = [ "agenixInstall" "users" "groups" ];
        };
      };

    systemd.services =
      let
        makeLinkScript = config: lib.concatStringsSep "\n" (map (location: ''mkdir -p "${location.path}"'') config);
        systemLinksScript = makeLinkScript (builtins.filter (link: !(link.file or false)) config.chvp.base.zfs.systemLinks);
        homeLinksScript = makeLinkScript (builtins.filter (link: !(link.file or false)) config.chvp.base.zfs.homeLinks);
      in
      {
        make-system-links-destinations = {
          script = systemLinksScript;
          after = [ "local-fs.target" ];
          wants = [ "local-fs.target" ];
          before = [ "shutdown.target" "sysinit.target" ];
          conflicts = [ "shutdown.target" ];
          wantedBy = [ "sysinit.target" ];
          serviceConfig = {
            RemainAfterExit = "yes";
            Type = "oneshot";
            UMask = "0077";
          };
          unitConfig = {
            DefaultDependencies = "no";
          };
        };

        make-home-links-destinations = {
          script = homeLinksScript;
          after = [ "local-fs.target" "make-system-links-destinations.service" ];
          wants = [ "local-fs.target" "make-system-links-destinations.service" ];
          before = [ "shutdown.target" "sysinit.target" ];
          conflicts = [ "shutdown.target" ];
          wantedBy = [ "sysinit.target" ];
          serviceConfig = {
            RemainAfterExit = "yes";
            Type = "oneshot";
            User = "charlotte";
            Group = "users";
            UMask = "0077";
            WorkingDirectory = "/home/charlotte";
          };
          unitConfig = {
            DefaultDependencies = "no";
          };
        };
      };

    systemd.mounts =
      (map
        (location: {
          what = "/${location.type}${location.path}";
          where = "${location.path}";
          type = "none";
          options = "bind";
          after = [ "local-fs.target" "make-system-links-destinations.service" ];
          wants = [ "local-fs.target" "make-system-links-destinations.service" ];
          before = [ "umount.target" "sysinit.target" ];
          conflicts = [ "umount.target" ];
          wantedBy = [ "sysinit.target" ];
          unitConfig = {
            DefaultDependencies = "no";
          };
        })
        config.chvp.base.zfs.systemLinks) ++
      (map
        (location: {
          what = "/${location.type}/home/charlotte/${location.path}";
          where = "/home/charlotte/${location.path}";
          type = "none";
          options = "bind";
          after = [ "local-fs.target" "make-home-links-destinations.service" ];
          wants = [ "local-fs.target" "make-home-links-destinations.service" ];
          before = [ "umount.target" "sysinit.target" ];
          conflicts = [ "umount.target" ];
          wantedBy = [ "sysinit.target" ];
          unitConfig = {
            DefaultDependencies = "no";
          };
        })
        config.chvp.base.zfs.homeLinks);
  };
}
