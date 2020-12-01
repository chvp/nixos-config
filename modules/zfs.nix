{ config, lib, ... }:
let
  linkCommands = map
    (location: ''
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "/home/charlotte/$(dirname ${location.path})"
      $DRY_RUN_CMD ln -sf -T $VERBOSE_ARG "/${location.type}/home/charlotte/${location.path}" "/home/charlotte/${location.path}"
    '')
    config.chvp.zfs.homeLinks;
in
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
  };

  config = lib.mkIf config.chvp.zfs.enable {
    chvp.dataPrefix = lib.mkDefault "/data";
    chvp.cachePrefix = lib.mkDefault "/cache";

    boot = {
      supportedFilesystems = [ "zfs" ];
      zfs.requestEncryptionCredentials = config.chvp.zfs.encrypted;
      initrd.postDeviceCommands = lib.mkAfter ''
        zfs rollback -r rpool/local/root@blank
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

    systemd.tmpfiles.rules = (
      [ "d /home/charlotte 0700 charlotte users - -" ] ++
      (map (location: "L ${location.path} - - - - /${location.type}${location.path}") config.chvp.zfs.systemLinks)
    );

    home-manager.users.charlotte = { lib, ... }: {
      home.activation = {
        linkCommands = lib.hm.dag.entryAfter [ "writeBoundary" ] (lib.concatStringsSep "\n" linkCommands);
      };
    };
  };
}
