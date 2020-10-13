{ config, lib, ... }:
let
  linkCommands = map
    (location: ''
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "/home/charlotte/$(dirname ${location.path})"
      $DRY_RUN_CMD ln -sf -T $VERBOSE_ARG "/${location.type}/home/charlotte/${location.path}" "/home/charlotte/${location.path}"
    '')
    config.custom.zfs.homeLinks;
in
{
  options.custom.zfs = {
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
  };

  config.boot = lib.mkIf config.custom.zfs.enable {
    supportedFilesystems = [ "zfs" ];
    zfs.requestEncryptionCredentials = config.custom.zfs.encrypted;
    initrd.postDeviceCommands = lib.mkAfter ''
      zfs rollback -r rpool/local/root@blank
    '';
  };

  config.virtualisation.docker.storageDriver = lib.mkIf config.custom.zfs.enable "zfs";

  config.services.zfs.autoScrub.enable = config.custom.zfs.enable;
  config.services.zfs.trim.enable = config.custom.zfs.enable;

  config.systemd.tmpfiles.rules = lib.mkIf config.custom.zfs.enable (
    [ "d /home/charlotte 0700 charlotte users - -" ] ++
    (map (location: "L ${location.path} - - - - /${location.type}${location.path}") config.custom.zfs.systemLinks)
  );

  config.home-manager.users.charlotte = { lib, ... }: {
    home.activation = lib.mkIf config.custom.zfs.enable {
      linkCommands = lib.hm.dag.entryAfter [ "writeBoundary" ] (lib.concatStringsSep "\n" linkCommands);
    };
  };
}
