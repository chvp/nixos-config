{ lib, pkgs, inputs, ... }:
pkgs.devshell.mkShell {
  name = "Silverfin";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  devshell.startup = {
    "link-devshell-dir".text = ''
      ln -sf $DEVSHELL_DIR $PRJ_DATA_DIR/devshell
    '';
  };
  packages = with pkgs; [
    cmake
    ghostscript
    graphicsmagick
    imagemagick6
    jq
    kubectl
    minikube
    mupdf-headless
    nodejs_18
    xsv
    yarn
    zlib
  ];
  env = [
    {
      name = "LIBRARY_PATH";
      eval = "$LD_LIBRARY_PATH";
    }
  ];
  language.c.compiler = lib.mkForce pkgs.clang;
  language.ruby = {
    package = pkgs.ruby_3_2;
    nativeDeps = with pkgs; [
      git
      graphicsmagick
      icu
      imagemagick6
      openssl
      postgresql
      libffi
      libyaml
      zlib
    ];
  };
}
