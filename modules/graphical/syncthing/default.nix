{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.syncthing.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.syncthing.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".config/syncthing"; type = "data"; }
      { path = "sync"; type = "cache"; }
    ];
    home-manager.users.charlotte = { pkgs, ... }: {
      services.syncthing.enable = true;
    };
  };
}
