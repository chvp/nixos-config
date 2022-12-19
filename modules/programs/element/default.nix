{ config, lib, pkgs, ... }:

{
  options.chvp.programs.element.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.element.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".config/Element"; type = "cache"; }
    ];
    home-manager.users.charlotte = { ... }: {
      home = {
        packages = [ pkgs.element-desktop ];
        sessionVariables = {
          # NIXOS_OZONE_WL = "1";
        };
      };
    };
  };
}
