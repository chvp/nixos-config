{ config, lib, pkgs, ... }:
let
  web = pkgs.mkYarnPackage rec {
    pname = "accentor-web";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "accentor";
      repo = "web";
      rev = "main";
      sha256 = "01izdmxwlyyxpcd2xvwf8hmh8g7m7wgnp9vf9f8ph84zhsd7l49s";
    };
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    yarnNix = ./yarn.nix;
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
    rev = "main";
    sha256 = "03vmn2x5j07cq7iqvslap05h20cq1wf5mn24pgahb8vk41k7l5sh";
  };
  gems = pkgs.bundlerEnv {
    name = "accentor-api-env";
    ruby = pkgs.ruby_3_0;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
    groups = [ "default" "development" "test" "production" ];
  };
  env = {
    DATABASE_URL = "postgresql://%2Frun%2Fpostgresql/accentor";
    FFMPEG_LOG_LOCATION = "/var/log/accentor/ffmpeg.log";
    FFMPEG_VERSION_LOCATION = "${config.chvp.dataPrefix}/var/lib/accentor/ffmpeg.version";
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
  options.chvp.services.accentor.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.accentor.enable {
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "accentor-console" ''
        set -ex
        export DATABASE_URL="postgresql://%2Frun%2Fpostgresql/accentor"
        export FFMPEG_LOG_LOCATION="/var/log/accentor/ffmpeg.log"
        export RAILS_STORAGE_PATH="${config.chvp.dataPrefix}/var/lib/accentor/storage"
        export FFMPEG_VERSION_LOCATION="${config.chvp.dataPrefix}/var/lib/accentor/ffmpeg.version"
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
          EnvironmentFile = config.age.secrets."passwords/services/accentor".path;
          Type = "simple";
          User = "accentor";
          Group = "accentor";
          Restart = "on-failure";
          WorkingDirectory = api;
          ExecStartPre = [
            "${gems}/bin/bundle exec rails db:migrate"
            "${gems}/bin/bundle exec rails ffmpeg:check_version"
          ];
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
            EnvironmentFile = config.age.secrets."passwords/services/accentor".path;
            Type = "simple";
            User = "accentor";
            Group = "accentor";
            Restart = "on-failure";
            WorkingDirectory = api;
            ExecStart = "${gems}/bin/bundle exec rails jobs:work";
          };
        };

      }) 4));

    age.secrets."passwords/services/accentor" = {
      file = ../../../secrets/passwords/services/accentor.age;
      owner = "accentor";
    };

    users.users.accentor = {
      group = "accentor";
      home = "${config.chvp.dataPrefix}/var/lib/accentor";
      createHome = true;
      uid = 314;
    };
    users.groups.accentor.gid = 314;

    chvp.services.nginx.hosts = [{
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
