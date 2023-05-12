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
    home.packages = with pkgs; [
      (yt-dlp.overrideAttrs (old: {
        version = "unstable";
        src = pkgs.fetchFromGitHub {
          owner = "yt-dlp";
          repo = "yt-dlp";
          rev = "b423b6a48e0b19260bc95ab7d72d2138d7f124dc";
          hash = "sha256-09eELJXIekLGVHjDr9XXPYjsew+ZsSkcAmcMF27mcv8=";
        };
        patches = (old.patches or [ ]) ++ [
          (pkgs.fetchpatch {
            url = "https://patch-diff.githubusercontent.com/raw/yt-dlp/yt-dlp/pull/6654.diff";
            hash = "sha256-TNOkhfCf02WZaHyTvlBKBfRre719mdScRJ0azyLTSyQ=";
          })
        ];
      }))
    ];
  };
}
