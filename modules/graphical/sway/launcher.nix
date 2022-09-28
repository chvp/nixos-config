{ pkgs, stdenv }:
let
  script = pkgs.substituteAll {
    src = ./launcher.zsh;
    inherit (pkgs)
      fzy
      jq
      libqalculate
      nix
      pass
      slurp
      sway
      uni
      zsh
      ;
    wfRecorder = pkgs.wf-recorder;
    wlClipboard = pkgs.wl-clipboard;
    xdgUserDirs = pkgs.xdg-user-dirs;
  };
in
pkgs.runCommand "launcher" { } ''
  mkdir -p $out/bin
  cp ${script} $out/bin/launcher
  chmod +x $out/bin/launcher
''
