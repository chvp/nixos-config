{ config, lib, pkgs, ... }:
let
  web = pkgs.mkYarnPackage rec {
    pname = "accentor-web";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "accentor";
      repo = "web";
      rev = "develop";
      sha256 = "1w5lp4p52dk9al0v1am3i7xpk9dxlfjqx7s3laadvy4p0vc4im79";
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
    sha256 = "0l1vd22n89j2dg1vl2j1rr4ga1w1ssfkl2716zpkwby3i0389xxp";
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
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "accentor-console" ''
        set -ex
        export DATABASE_URL="postgresql://%2Frun%2Fpostgresql/accentor"
        export FFMPEG_LOG_LOCATION="/var/log/accentor/ffmpeg.log"
        export RAILS_STORAGE_PATH="${config.chvp.dataPrefix}/var/lib/accentor/storage"
        export RAILS_TRANSCODE_CACHE="/var/tmp/accentor/transcode_cache"
        export BOOTSNAP_CACHE_DIR="/var/tmp/accentor/bootsnap"
        export PIDFILE="/run/accentor/server.pid"
        export RACK_ENV="production"
        export RAILS_ENV="production"
        export RAILS_LOG_TO_STDOUT="yes"
        cd ${api}
        ${gems}/bin/bundle exec rails c
      '')
    ];

    security.doas.extraRules = [{
      users = [ "charlotte" ];
      noPass = true;
      cmd = "accentor-console";
      runAs = "accentor";
      setEnv = [ "RAILS_MASTER_KEY" ];
    }];

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
    } // (builtins.foldl' (x: y: x // y) { } (builtins.genList
      (n: {
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
