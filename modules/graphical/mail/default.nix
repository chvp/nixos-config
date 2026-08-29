{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
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
    home-manager.users.${username} = {
      home.packages = [ pkgs.thunderbird ];
    };
  };
}
