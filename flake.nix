{
  description = "Nixos configuration flake";

  inputs = {
    accentor = {
      url = "github:accentor/flake";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    accentor-api = {
      url = "github:accentor/api";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    accentor-web = {
      url = "github:accentor/web";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    tetris = {
      url = "github:chvp/tetris";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    utils = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs = {
        flake-utils.follows = "flake-utils";
        devshell.follows = "devshell";
      };
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs@{ self, nixpkgs, accentor, accentor-api, accentor-web, agenix, devshell, emacs-overlay, flake-utils, home-manager, nixos-mailserver, nur, rust-overlay, tetris, utils }:
    let
      customPackages = callPackage: {
        jdtls = callPackage ./packages/jdtls { };
        kotlin-language-server = callPackage ./packages/kotlin-language-server { };
      }; in
    utils.lib.mkFlake {
      inherit self inputs;
      channels.nixpkgs = {
        input = nixpkgs;
        patches = map (patch: ./patches + "/${patch}") (builtins.filter (x: x != ".keep") (builtins.attrNames (builtins.readDir ./patches)));
        overlaysBuilder = _: [
          devshell.overlay
          emacs-overlay.overlay
          (self: super: customPackages self.callPackage)
          (self: super: {
            tetris = tetris.packages.${self.system}.default;
            accentor-api = accentor-api.packages.${self.system}.default;
            accentor-web = accentor-web.packages.${self.system}.default;
          })
          nur.overlay
          rust-overlay.overlay
        ];
      };
      hostDefaults = {
        modules = [
          { nix.generateRegistryFromInputs = true; }
          accentor.nixosModule
          agenix.nixosModules.age
          home-manager.nixosModule
          nixos-mailserver.nixosModule
          ./modules
        ];
      };
      hosts = {
        kharbranth.modules = [ ./machines/kharbranth ];
        kholinar.modules = [ ./machines/kholinar ];
        lasting-integrity.modules = [ ./machines/lasting-integrity ];
        urithiru.modules = [ ./machines/urithiru ];
      };
      outputsBuilder = channels:
        let pkgs = channels.nixpkgs; in
        {
          packages = customPackages pkgs.callPackage;
          devShells = rec {
            default = nixos-config;
            nixos-config = pkgs.devshell.mkShell {
              name = "NixOS config";
              packages = [
                pkgs.nixpkgs-fmt
                (pkgs.writeShellScriptBin "fetchpatch" "curl -L https://github.com/NixOS/nixpkgs/pull/$1.patch -o patches/$1.patch")
                agenix.defaultPackage.x86_64-linux
              ];
            };
            accentor-api-client-js = pkgs.devshell.mkShell {
              name = "Accentor API client in JavaScript";
              packages = with pkgs; [ nodejs yarn ];
            };
            gamification2 = pkgs.devshell.mkShell {
              name = "Gamification 2";
              imports = [ "${devshell}/extra/language/c.nix" ];
              packages = with pkgs; [
                (pkgs.lowPrio binutils)
                findutils
                cmake
                gnumake
                nodejs
                postgresql_14
                ruby_3_1
                yarn
              ];
              env = [
                { name = "PGDATA"; eval = "$PRJ_DATA_DIR/postgres"; }
                { name = "DATABASE_HOST"; eval = "$PGDATA"; }
                { name = "GEM_HOME"; eval = "$PRJ_DATA_DIR/bundle/$(ruby -e 'puts RUBY_VERSION')"; }
                { name = "PATH"; prefix = "$GEM_HOME/bin"; }
              ];
              commands = [
                {
                  name = "pg:setup";
                  category = "database";
                  help = "Setup postgres in project folder";
                  command = ''
                    initdb --encoding=UTF8 --no-locale --no-instructions -U postgres
                    echo "listen_addresses = ${"'"}${"'"}" >> $PGDATA/postgresql.conf
                    echo "unix_socket_directories = '$PGDATA'" >> $PGDATA/postgresql.conf
                    echo "CREATE USER gamification2 WITH PASSWORD 'gamification2' CREATEDB;" | postgres --single -E postgres
                  '';
                }
                {
                  name = "pg:start";
                  category = "database";
                  help = "Start postgres instance";
                  command = ''
                    [ ! -d $PGDATA ] && pg:setup
                    pg_ctl -D $PGDATA -U postgres start -l log/postgres.log
                  '';
                }
                {
                  name = "pg:stop";
                  category = "database";
                  help = "Stop postgres instance";
                  command = ''
                    pg_ctl -D $PGDATA -U postgres stop
                  '';
                }
                {
                  name = "pg:console";
                  category = "database";
                  help = "Open database console";
                  command = ''
                    psql --host $PGDATA -U postgres
                  '';
                }
              ];
              language.c = {
                compiler = pkgs.gcc;
                includes = [ pkgs.zlib pkgs.openssl ];
                libraries = [ pkgs.zlib pkgs.openssl ];
              };
            };
            tab = pkgs.devshell.mkShell {
              name = "Tab";
              imports = [ "${devshell}/extra/language/c.nix" ];
              packages = with pkgs; [
                (pkgs.lowPrio binutils)
                findutils
                gnumake
                ruby_3_1
                nodejs
                yarn
              ];
              env = [
                { name = "GEM_HOME"; eval = "$PRJ_DATA_DIR/bundle/$(ruby -e 'puts RUBY_VERSION')"; }
                { name = "PATH"; prefix = "$GEM_HOME/bin"; }
              ];
              commands = [
                {
                  name = "server-support";
                  category = "general commands";
                  help = "Run everything required for a server";
                  command = ''
                    bundle install
                  '';
                }
                {
                  name = "server";
                  category = "general commands";
                  help = "Run everything";
                  command = ''
                    server-support
                    rails s
                  '';
                }
              ];
              language.c = {
                compiler = pkgs.gcc;
                includes = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
                libraries = [ pkgs.sqlite pkgs.libmysqlclient pkgs.zlib ];
              };
            };
            dodona = pkgs.devshell.mkShell {
              name = "Dodona";
              imports = [ "${devshell}/extra/language/c.nix" ];
              packages = with pkgs; [
                (pkgs.lowPrio binutils)
                chromedriver
                findutils
                gnumake
                nodejs
                nodePackages.typescript-language-server
                ruby_3_0
                yarn
              ];
              env = [
                { name = "DATABASE_URL"; value = "mysql2://root:dodona@127.0.0.1:3306/dodona"; }
                { name = "TEST_DATABASE_URL"; value = "mysql2://root:dodona@127.0.0.1:3306/dodona_test"; }
                { name = "GEM_HOME"; eval = "$PRJ_DATA_DIR/bundle/$(ruby -e 'puts RUBY_VERSION')"; }
                { name = "PATH"; prefix = "$GEM_HOME/bin"; }
              ];
              commands = [
                {
                  name = "memcached";
                  category = "general commands";
                  help = "Start caching server";
                  package = pkgs.memcached;
                }
                {
                  name = "mysql";
                  category = "general commands";
                  help = "Start mysql (in docker container)";
                  command = ''
                    trap "systemd-run --user --no-block docker stop dodona-db" 0
                    docker run -d --name dodona-db -p 3306:3306 --rm -v dodona-db-data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest
                    while [ 1 -eq 1 ]
                    do
                      sleep 1000
                    done
                  '';
                }
                {
                  name = "server-support";
                  category = "general commands";
                  help = "Run everything required for a server";
                  command = ''
                    memcached &
                    mysql &
                    bundle install
                    yarn install
                    rails jobs:work &
                    yarn build:css --watch &
                    yarn build:js --watch
                  '';
                }
                {
                  name = "server";
                  category = "general commands";
                  help = "Run everything";
                  command = ''
                    server-support &
                    rails s
                  '';
                }
              ];
              language.c = {
                compiler = pkgs.gcc;
                includes = [ pkgs.libmysqlclient pkgs.zlib ];
                libraries = [ pkgs.libmysqlclient pkgs.zlib ];
              };
            };
            dodona-docs = pkgs.devshell.mkShell {
              name = "Dodona Docs";
              env = [{ name = "PUPPETEER_EXECUTABLE_PATH"; eval = "${pkgs.ungoogled-chromium}/bin/chromium"; }];
              packages = with pkgs; [ nodejs yarn ];
            };
            dodona-judge-r = pkgs.devshell.mkShell {
              name = "R judge";
              packages = [
                (pkgs.rWrapper.override {
                  packages = with pkgs.rPackages; [ base64enc dplyr dslabs jsonlite knitr lintr R6 rlang styler ];
                })
              ];
            };
            scriptingtalen-project = pkgs.devshell.mkShell {
              name = "Scriptingtalen project";
              packages = [ (pkgs.python3.withPackages (ps: with ps; [ beautifulsoup4 requests ])) ];
            };
            Rocket = pkgs.devshell.mkShell {
              name = "Rocket";
              imports = [ "${devshell}/extra/language/c.nix" ];
              env = [
                { name = "PQ_LIB_DIR"; value = "${pkgs.postgresql.lib}/lib"; }
              ];
              packages = with pkgs; [
                binutils
                (rust-bin.nightly.latest.default.override { extensions = [ "rust-analyzer-preview" "rust-src" ]; })
              ];
              language.c = {
                compiler = pkgs.gcc;
                includes = [ pkgs.postgresql.lib pkgs.sqlite pkgs.libmysqlclient pkgs.openssl ];
                libraries = [ pkgs.postgresql.lib pkgs.sqlite pkgs.libmysqlclient pkgs.openssl ];
              };
            };
          };
        };
    };
}
