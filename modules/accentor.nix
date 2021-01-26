{ config, lib, pkgs, ... }:
let
  web = pkgs.mkYarnPackage rec {
    pname = "accentor-web";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "accentor";
      repo = "web";
      rev = "develop";
      sha256 = "0amwp8zzi8mz3k8fvf50zxl88haxxxl9dkwv2ll0swzp5n2ysmks";
    };
    yarnNix = ./accentor/yarn.nix;
    buildPhase = ''
      cp deps/accentor/postcss.config.js .
      yarn run build
    '';
    installPhase = ''
      cp -r deps/accentor/dist $out
      rm $out/**/*.map
    '';
    distPhase = "true";
  };
  api = pkgs.fetchFromGitHub {
    owner = "accentor";
    repo = "api";
    rev = "develop";
    sha256 = "0fqxz33cxw3k4swm2rh5q8yxmnq6wny6pv4gy467rsdqvj8h5l1f";
  };
  gems = pkgs.bundlerEnv {
    name = "accentor-api-env";
    ruby = pkgs.ruby_2_7;
    gemfile = ./accentor/Gemfile;
    lockfile = ./accentor/Gemfile.lock;
    gemset = ./accentor/gemset.nix;
    groups = [ "default" "development" "test" "production" ];
  };
  env = {
    DATABASE_URL = "postgresql://%2Frun%2Fpostgresql/accentor";
    FFMPEG_LOG_LOCATION = "/var/log/accentor/ffmpeg.log";
    RAILS_STORAGE_PATH = "${config.chvp.dataPrefix}/var/lib/accentor/storage";
    RAILS_TRANSCODE_CACHE = "/var/tmp/accentor/transcode_cache";
    BOOTSNAP_CACHE_DIR = "/var/tmp/accentor/bootsnap";
    PIDFILE = "/run/accentor/server.pid";
    RACK_ENV = "production";
    RAILS_ENV = "production";
    RAILS_LOG_TO_STDOUT = "yes";
  };
in
{
  options.chvp.accentor.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.accentor.enable {
    services.postgresql = {
      enable = true;
      dataDir = "${config.chvp.dataPrefix}/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
      ensureUsers = [{
        name = "accentor";
        ensurePermissions = { "DATABASE accentor" = "ALL PRIVILEGES"; };
      }];
      ensureDatabases = [ "accentor" ];
    };

    systemd.tmpfiles.rules = [
      "d /run/accentor 0755 accentor accentor -"
      "d /var/log/accentor 0755 accentor accentor -"
      "d /var/tmp/accentor/transcode_cache 0755 accentor accentor -"
      "d /var/tmp/accentor/bootsnap 0755 accentor accentor -"
      "d ${config.chvp.dataPrefix}/var/lib/accentor/storage 0755 accentor accentor -"
    ];

    systemd.services = {
      accentor-api = {
        after = [ "network.target" "postgresql.service" ];
        requires = [ "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];
        environment = env;
        path = [ pkgs.ffmpeg gems gems.wrappedRuby ];
        serviceConfig = {
          EnvironmentFile = "${config.chvp.dataPrefix}/var/secrets/accentor-api";
          Type = "simple";
          User = "accentor";
          Group = "accentor";
          Restart = "on-failure";
          WorkingDirectory = api;
          ExecStartPre = "${gems}/bin/bundle exec rails db:migrate";
          ExecStart = "${gems}/bin/bundle exec puma -C ${api}/config/puma.rb";
        };
      };
    } // (builtins.foldl' (x: y: x // y) {} (builtins.genList (n: {
      "accentor-worker${toString n}" = {
        after = [ "network.target" "accentor-api.service" "postgresql.service" ];
        requires = [ "accentor-api.service" "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];
        environment = env;
        path = [ pkgs.ffmpeg gems gems.wrappedRuby ];
        serviceConfig = {
          EnvironmentFile = "${config.chvp.dataPrefix}/var/secrets/accentor-api";
          Type = "simple";
          User = "accentor";
          Group = "accentor";
          Restart = "on-failure";
          WorkingDirectory = api;
          ExecStart = "${gems}/bin/bundle exec rails jobs:work";
        };
      };

    }) 4));

    users.users.accentor = {
      group = "accentor";
      home = "${config.chvp.dataPrefix}/var/lib/accentor";
      createHome = true;
      uid = 314;
    };
    users.groups.accentor.gid = 314;

    chvp.nginx.hosts = [{
      fqdn = "accentor.vanpetegem.me";
      options = {
        root = web;
        locations = {
          "/api" = {
            proxyPass = "http://localhost:3000";
            extraConfig = ''
              proxy_set_header X-Forwarded-Ssl on;
              client_max_body_size 40M;
            '';
          };
          "/rails" = {
            proxyPass = "http://localhost:3000";
            extraConfig = ''
              proxy_set_header X-Forwarded-Ssl on;
            '';
          };
          "/".extraConfig = ''
            autoindex on;
            try_files $uri $uri/ /index.html =404;
          '';
        };
      };
    }];
  };
}
