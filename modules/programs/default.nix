{ config, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./calibre
    ./eid
    ./element
    ./hledger
    ./htop
    ./torrents
  ];

  home-manager.users.${username} = {
    home.packages = with pkgs; [
      jq
      xan
      yt-dlp
      libqalculate
    ];
  };
}
