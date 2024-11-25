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
  commands = [
    {
      name = "delete-merged";
      category = "[general commands]";
      help = "Delete merged branches";
      command = ''
        git fetch -p ; git branch -r | awk '{print $1}' | egrep -v -f - <(git branch -vv | grep origin) | awk '{print $1}' | xargs -r git branch -D
      '';
    }
    {
      name = "reset-test-assets";
      category = "[general commands]";
      help = "Force reset test assets";
      command = ''
        RAILS_ENV=test bundle exec rake webpacker:clobber && RAILS_ENV=test bundle exec rake webpacker:compile
      '';
    }
    {
      name = "reset-dev-translations-js";
      category = "[general commands]";
      help = "Force reset translations for JS inclusion";
      command = ''
        WEBPACKER_PRECOMPILE=false bundle exec rails assets:precompile
      '';
    }
  ];
  packages = with pkgs; [
    chromedriver
    cmake
    ghostscript
    google-cloud-sdk
    graphicsmagick
    imagemagick6
    jq
    kubectl
    llvmPackages.libllvm
    minikube
    mupdf-headless
    nodejs_18
    poppler_utils
    (pkgs.lowPrio postgresql)
    stern
    xsv
    yarn
    zlib
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
  ];
  language.c.compiler = lib.mkForce pkgs.clang;
  language.ruby = {
    package = pkgs.ruby_3_2.overrideAttrs (old: {
      version = (import "${inputs.nixpkgs}/pkgs/development/interpreters/ruby/ruby-version.nix" { inherit lib; }) "3" "2" "4" "";
      src = pkgs.fetchurl {
          url = "https://cache.ruby-lang.org/pub/ruby/3.2/ruby-3.2.4.tar.gz";
          hash = "sha256-xys8XDBILcoYsPhoyQdfP0fYFo6vYm1OaCzltZyFhpI=";
      };
    });
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
