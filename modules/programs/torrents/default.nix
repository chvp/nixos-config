{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.programs.torrents.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.torrents.enable {
    chvp.base.zfs.homeLinks = [{ path = ".config/transmission-remote-gtk"; type = "data"; }];

    home-manager.users.${username}.home.packages = with pkgs; [ transmission-remote-gtk ];
  };
}
