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
            hash = "sha256-ezJjZyxaSsPfXRoyxL2FQhbO1YUtwhb6mM0atFfO3JY=";
          })
        ];
      }))
    ];
  };
}
