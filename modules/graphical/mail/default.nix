{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.mail.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.mail.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".cache/thunderbird"; type = "cache"; }
      { path = ".thunderbird"; type = "cache"; }
    ];
    home-manager.users.charlotte = { lib, ... }: {
      home.packages = [ pkgs.thunderbird ];
    };
  };
}
