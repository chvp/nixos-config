{ config, lib, pkgs, ... }:

{
  options.chvp.programs.deluge.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.deluge.enable {
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ deluge ];
    };

    chvp.base.zfs.homeLinks = [
      { path = ".config/deluge"; type = "data"; }
    ];
  };
}
