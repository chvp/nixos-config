{ config, lib, ... }:

{
  options.chvp.zfs = {
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
        { path = ".gnupg/pubring.kbx"; type = "data"; file = true; }
      ];
    };
    backups = lib.mkOption {
      default = [ ];
      example = [{
        path = "rpool/safe/data";
        remotePath = "zdata/recv/<hostname>/safe/data";
        fast = false;
        location = "lasting-integrity.vanpetegem.me";
      }];
    };
    rootDataset = lib.mkOption {
      example = "rpool/local/root";
    };
  };

  config = lib.mkIf config.chvp.zfs.enable {
    chvp.dataPrefix = lib.mkDefault "/data";
    chvp.cachePrefix = lib.mkDefault "/cache";

    boot = {
      supportedFilesystems = [ "zfs" ];
      zfs.requestEncryptionCredentials = config.chvp.zfs.encrypted;
      initrd.postDeviceCommands = lib.mkAfter ''
        zfs rollback -r ${config.chvp.zfs.rootDataset}@blank
      '';
    };

    services = {
      znapzend = {
        enable = config.chvp.zfs.backups != [ ];
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
            config.chvp.zfs.backups);

      };
      zfs = {
        autoScrub.enable = true;
        trim.enable = true;
      };
    };

    systemd.services =
      let
        makeLinkScript = config: lib.strings.concatStringsSep "\n" (map
          (location:
            if location.file or false then
              ''
                mkdir -p $(dirname "${location.path}")
                [ -f "${location.path}" ] || touch "${location.path}"
              ''
            else
              ''mkdir -p "${location.path}"''
          )
          config);
        systemLinksScript = makeLinkScript config.chvp.zfs.systemLinks;
        homeLinksScript = makeLinkScript config.chvp.zfs.homeLinks;
      in
      {
        make-system-links-destinations = {
          script = ''
            ${systemLinksScript}
            mkdir -p /home/charlotte
            chown charlotte:users /home/charlotte
          '';
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
        config.chvp.zfs.systemLinks) ++
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
        config.chvp.zfs.homeLinks);
  };
}
