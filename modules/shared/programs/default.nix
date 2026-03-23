{ config, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./hledger
    ./htop
  ];

  home-manager.users.${username}.home.packages = with pkgs; [ jq xan yt-dlp libqalculate ];
}
