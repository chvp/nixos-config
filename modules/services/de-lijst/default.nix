{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.chvp.services.de-lijst.enable = lib.mkEnableOption "de-lijst";

  config = lib.mkIf config.chvp.services.de-lijst.enable (
    let
      serverPackage = pkgs.de-lijst;
      gemsPackage = serverPackage.env;
      environmentFile = config.age.secrets."passwords/services/de-lijst".path;
      home = "/var/lib/de-lijst";
      env = {
        BOOTSNAP_READONLY = "TRUE";
        DATABASE_URL = "postgresql://de-lijst?host=/run/postgresql";
        PIDFILE = "/run/de-lijst/server.pid";
        RACK_ENV = "production";
        RAILS_ENV = "production";
        RAILS_LOG_TO_STDOUT = "yes";
        RUBY_ENABLE_YJIT = "1";
      };
      exports = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (name: value: "export ${name}=\"${value}\"") env
      );
      console = pkgs.writeShellScriptBin "de-lijst-console" ''
        ${exports}
        export $(cat ${environmentFile} | xargs)
        cd ${serverPackage}
        ${gemsPackage}/bin/bundle exec rails c
      '';
      shell = pkgs.writeShellScriptBin "de-lijst-shell" ''
        ${exports}
        export $(cat ${environmentFile} | xargs)
        export PATH="${gemsPackage}/bin/:$PATH"
        cd ${serverPackage}
        bash
      '';
    in
    {
      environment.systemPackages = [
        console
        shell
      ];
      systemd.tmpfiles.rules = [
        "d /run/de-lijst 0755 de-lijst de-lijst -"
      ];
      systemd.services = {
        de-lijst = {
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
          environment = env;
          path = [
            gemsPackage
            gemsPackage.wrappedRuby
          ];
          serviceConfig = {
            EnvironmentFile = environmentFile;
            Type = "simple";
            User = "de-lijst";
            Group = "de-lijst";
            Restart = "on-failure";
            WorkingDirectory = serverPackage;
            ExecStartPre = [
              "${gemsPackage}/bin/bundle exec rails db:prepare"
              "${gemsPackage}/bin/bundle exec rails db:migrate"
            ];
            ExecStart = "${gemsPackage}/bin/puma -C ${serverPackage}/config/puma.rb";
          };
        };
      };
      users.users.de-lijst = {
        group = "de-lijst";
        home = home;
        createHome = true;
        uid = 356;
      };
      users.groups.de-lijst.gid = 356;

      services = {
        postgresql = {
          enable = true;
          ensureDatabases = [
            "de-lijst"
          ];
          ensureUsers = [
            {
              name = "de-lijst";
              ensureDBOwnership = true;
            }
          ];
        };
        nginx = {
          virtualHosts."de-lijst.chvp.be" = {
            forceSSL = true;
            useACMEHost = "vanpetegem.me";
            root = "${serverPackage}/public";
            locations = {
              "/" = {
                tryFiles = "$uri @app";
              };
              "@app" = {
                proxyPass = "http://localhost:3000";
                extraConfig = ''
                  	                  proxy_set_header X-Forwarded-Ssl on;
                  	                  client_max_body_size 40M;
                '';
              };
            };
          };
        };
      };

      security.doas.extraRules = [
        {
          users = [ "charlotte" ];
          noPass = true;
          cmd = "de-lijst-console";
          runAs = "de-lijst";
        }
        {
          users = [ "charlotte" ];
          noPass = true;
          cmd = "de-lijst-shell";
          runAs = "de-lijst";
        }
      ];

      age.secrets."passwords/services/de-lijst" = {
        file = ../../../secrets/passwords/services/de-lijst.age;
        owner = "de-lijst";
      };
    }
  );
}
