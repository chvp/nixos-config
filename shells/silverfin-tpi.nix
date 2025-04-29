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
    nodejs_20
    (pkgs.lowPrio postgresql_14)
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
      name = "DISABLE_SPRING";
      value = "1";
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
    package = pkgs.ruby_3_2.overrideAttrs (old: {
      version = (import "${inputs.nixpkgs}/pkgs/development/interpreters/ruby/ruby-version.nix" { inherit lib; }) "3" "2" "8" "";
      src = pkgs.fetchurl {
        url = "https://cache.ruby-lang.org/pub/ruby/3.2/ruby-3.2.8.tar.gz";
        hash = "sha256-d6zdjPu+H45XO15lNuA8UQPfmJ3AX6aMcPARgzw1YHU=";
      };
    });
    nativeDeps = with pkgs; [
      postgresql_14
      libffi
      zlib
    ];
  };
}
