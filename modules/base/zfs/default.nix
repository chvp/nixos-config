{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.base.zfs = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    encrypted = lib.mkOption {
      default = false;
      example = true;
    };
    systemLinks = lib.mkOption {
      default = [ ];
      example = [
        { path = "/var/lib/docker"; type = "cache"; }
        { path = "/var/lib/docker/volumes"; type = "data"; }
      ];
    };
    homeLinks = lib.mkOption {
      default = [ ];
      example = [
        { path = ".config/syncthing"; type = "data"; }
        { path = ".cache/nix-index"; type = "cache"; }
      ];
    };
    ensureSystemExists = lib.mkOption {
      default = [ ];
      example = [ "/data/etc/ssh" ];
    };
    ensureHomeExists = lib.mkOption {
      default = [ ];
      example = [ ".ssh" ];
    };
    backups = lib.mkOption {
      default = [ ];
      example = [{
        path = "rpool/safe/data";
        remotePath = "zdata/recv/<hostname>/safe/data";
        fast = false;
        location = "marabethia.vanpetegem.me";
      }];
    };
    rootDataset = lib.mkOption {
      example = "rpool/local/root";
    };
    rootPool = lib.mkOption {
      example = "rpool";
    };
  };

  config = lib.mkIf config.chvp.base.zfs.enable {
    chvp.dataPrefix = lib.mkDefault "/data";
    chvp.cachePrefix = lib.mkDefault "/cache";

    boot = {
      supportedFilesystems = [ "zfs" ];
      zfs = {
        forceImportRoot = false;
        requestEncryptionCredentials = config.chvp.base.zfs.encrypted;
      };
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
        ensureHomeExistsScript = lib.concatStringsSep "\n" (map (path: ''mkdir -p "/home/${username}/${path}"'') config.chvp.base.zfs.ensureHomeExists);
        ensureHomePermissionsScript = lib.concatStringsSep "\n" (map (path: ''chown ${username}:users /home/${username}/${path}'') config.chvp.base.zfs.ensureHomeExists);
      in
      {
        ensureSystemPathsExist = {
          text = ensureSystemExistsScript;
          deps = [ "agenixNewGeneration" ];
        };
        ensureHomePathsExist = {
          text = ''
            mkdir -p /home/${username}/
            ${ensureHomeExistsScript}
          '';
        };
        agenixInstall.deps = [ "ensureSystemPathsExist" "ensureHomePathsExist" ];
        ensureHomePermissionsScript = {
          text = ''
            chown ${username}:users /home/${username}
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
            User = "${username}";
            Group = "users";
            UMask = "0077";
            WorkingDirectory = "/home/${username}";
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
          what = "/${location.type}/home/${username}/${location.path}";
          where = "/home/${username}/${location.path}";
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
