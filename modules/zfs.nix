{ config, lib, ... }:

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

  config.services.zfs.autoScrub.enable = config.custom.zfs.enable;
  config.services.zfs.trim.enable = config.custom.zfs.enable;

  config.systemd.tmpfiles.rules = lib.mkIf config.custom.zfs.enable (
    [ "d /home/charlotte 0700 charlotte users - -" ] ++
    (map (location: "L ${location.path} - - - - /${location.type}${location.path}") config.custom.zfs.systemLinks)
  );

  config.home-manager.users.charlotte = { ... }: {
    systemd.user.tmpfiles.rules = lib.mkIf config.custom.zfs.enable (
      map
        (location: "L /home/charlotte/${location.path} - - - - /${location.type}/home/charlotte/${location.path}")
        config.custom.zfs.homeLinks
    );
  };
}
