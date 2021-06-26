{ config, lib, pkgs, ... }:

{
  options.chvp.syncthing-client.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.syncthing-client.enable {
    chvp.zfs.homeLinks = [
      { path = ".config/syncthing"; type = "data"; }
      { path = "sync"; type = "cache"; }
    ];
    home-manager.users.charlotte = { pkgs, ... }: {
      services.syncthing.enable = true;
    };
  };
}
