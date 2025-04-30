{ pkgs, stdenv }:
let
  script = pkgs.replaceVars ./launcher.zsh {
    inherit (pkgs)
      fzy
      libqalculate
      nix
      uni
      zsh
      ;
    wlClipboard = pkgs.wl-clipboard;
  };
in
pkgs.runCommand "launcher" { } ''
  mkdir -p $out/bin
  cp ${script} $out/bin/launcher
  chmod +x $out/bin/launcher
''
