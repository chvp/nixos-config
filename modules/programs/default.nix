{ pkgs, ... }:

{
  imports = [
    ./calibre
    ./deluge
    ./eid
    ./element
    ./hledger
    ./obs
  ];

  home-manager.users.charlotte = { ... }: {
    home.packages = with pkgs; [ yt-dlp ];
  };
}
