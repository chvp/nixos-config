{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.firefox.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.firefox.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".cache/mozilla"; type = "cache"; }
      { path = ".mozilla"; type = "data"; }
    ];

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [ firefox ];
    };
  };
}
