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
      (yt-dlp.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          (pkgs.fetchpatch {
            url = "https://patch-diff.githubusercontent.com/raw/yt-dlp/yt-dlp/pull/6654.diff";
            hash = "sha256-o9T5xGbrP/OgXUGw0s8fiD3+D63leF+OrrLFRBKN7SM=";
          })
        ];
      }))
    ];
  };
}
