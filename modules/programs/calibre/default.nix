{ config, lib, pkgs, ... }:

{
  options.chvp.programs.calibre.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.calibre.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".config/calibre"; type = "cache"; }
    ];
    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.calibre ];
    };
    services.udisks2.enable = true;
  };
}
