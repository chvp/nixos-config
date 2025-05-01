{ pkgs, stdenv }:
let
  script = pkgs.replaceVars ./launcher.zsh {
    inherit (pkgs)
      fzy
      libqalculate
      nix
      uni
      wl-clipboard
      zsh
      ;
  };
in
pkgs.runCommand "launcher" { } ''
  mkdir -p $out/bin
  cp ${script} $out/bin/launcher
  chmod +x $out/bin/launcher
''
