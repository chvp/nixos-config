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
        patches = (old.patches or [ ]) ++ [
          (fetchpatch {
            url = https://github.com/yt-dlp/yt-dlp/pull/6654.patch;
            hash = "sha256-r7pY7RzF1e2otQmyha6wX3Q9s7H8PllDj1WeriJ2cVs=";
          })
        ];
      }))
    ];
  };
}
