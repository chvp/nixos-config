{ config, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./hledger
  ];

  home-manager.users.${username}.home.packages = with pkgs; [ jq yt-dlp ];
}
