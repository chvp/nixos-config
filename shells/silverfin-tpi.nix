{ lib, pkgs, inputs, ... }:
pkgs.devshell.mkShell {
  name = "Silverfin";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  devshell = {
    motd = "";
    startup = {
      # Hack to make sure Rubymine doesn't use an ephemeral path from the nix store
      "link-devshell-dir".text = ''
        ln -snf $DEVSHELL_DIR $PRJ_DATA_DIR/devshell
      '';
    };
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
    # Workaround for Rubymine not setting a TERM and some applications not being able to handle that
    {
      name = "TERM";
      eval = "\${TERM:-xterm-256color}";
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
