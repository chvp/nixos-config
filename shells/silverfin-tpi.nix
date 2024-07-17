{ lib, pkgs, inputs, ... }:
pkgs.devshell.mkShell {
  name = "Silverfin";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  devshell.startup = {
    "link-devshell-dir".text = ''
      ln -snf $DEVSHELL_DIR $PRJ_DATA_DIR/devshell
    '';
  };
  packages = with pkgs; [
    cmake
    nodejs_18
    shared-mime-info
    yarn
  ];
  env = [
    {
      name = "LIBRARY_PATH";
      eval = "$LD_LIBRARY_PATH";
    }
    {
      name = "POSTGRESQL_ADDRESS";
      value = "localhost";
    }
    {
      name = "POSTGRESQL_USERNAME";
      value = "silverfin";
    }
    {
      name = "POSTGRESQL_PASSWORD";
      value = "silverfin";
    }
  ];
  language.c.compiler = lib.mkForce pkgs.clang;
  language.ruby = {
    package = pkgs.ruby_3_2;
    nativeDeps = with pkgs; [
      postgresql
      libffi
      zlib
    ];
  };
}
