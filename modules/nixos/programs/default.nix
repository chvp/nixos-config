{ pkgs, ... }:

{
  imports = [
    ./calibre
    ./eid
    ./element
    ./hledger
    ./obs
    ./torrents
  ];

  home-manager.users.charlotte = { ... }: {
    home.packages = with pkgs; [
      jq
      xsv
      yt-dlp
    ];
  };
}
