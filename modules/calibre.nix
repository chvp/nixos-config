{ config, lib, pkgs, ... }:

{
  options.chvp.calibre.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.calibre.enable {
    chvp.zfs.homeLinks = [
      { path = ".config/calibre"; type = "cache"; }
    ];
    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.calibre ];
    };
  };
}
