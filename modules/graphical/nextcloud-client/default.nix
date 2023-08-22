{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.nextcloud-client.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.nextcloud-client.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".config/Nextcloud"; type = "cache"; }
      { path = ".local/share/Nextcloud"; type = "cache"; }
      { path = "sync"; type = "cache"; }
    ];
    home-manager.users.charlotte = { ... }: {
      services.nextcloud-client = {
        enable = true;
        startInBackground = true;
      };
    };
  };
}
