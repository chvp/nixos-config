{ pkgs, wrappedBemenu }:
let
  script = pkgs.replaceVars ./launcher.zsh {
    inherit (pkgs)
      darkman
      foot
      nix
      uni
      wl-clipboard
      zsh
      ;
    bemenu = wrappedBemenu;
  };
in
pkgs.runCommand "launcher" { } ''
  mkdir -p $out/bin
  cp ${script} $out/bin/launcher
  chmod +x $out/bin/launcher
''
