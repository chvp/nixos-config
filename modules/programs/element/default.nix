{ config, lib, pkgs, ... }:

{
  options.chvp.programs.element.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.element.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".config/nheko"; type = "data"; }
      { path = ".local/share/nheko"; type = "data"; }
      { path = ".cache/nheko"; type = "cache"; }
    ];
    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.nheko ];
    };
  };
}
