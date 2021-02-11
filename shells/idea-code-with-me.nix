let
  pkgs = import <nixpkgs> {};
in
  (pkgs.buildFHSUserEnv {
    name = "idea-env";
    targetPkgs = pkgs: [ pkgs.jdk11 pkgs.wget pkgs.bash pkgs.zlib pkgs.xlibs.libXext pkgs.xlibs.libX11 pkgs.xlibs.libXrender pkgs.xlibs.libXtst pkgs.xlibs.libXi pkgs.freetype pkgs.fontconfig ];
  }).env
