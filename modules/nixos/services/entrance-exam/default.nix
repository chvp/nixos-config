{ config, lib, pkgs, ... }:

{
  options.chvp.services.entrance-exam.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.entrance-exam.enable (
    let
      serverPackage = pkgs.entrance-exam;
      gemsPackage = serverPackage.env;
      environmentFile = config.age.secrets."passwords/services/entrance-exam".path;
      home = "/var/lib/entrance-exam";
      env = {
        BOOTSNAP_READONLY = "TRUE";
        PIDFILE = "/run/entrance-exam/server.pid";
        RACK_ENV = "production";
        RAILS_ENV = "production";
        RAILS_LOG_TO_STDOUT = "yes";
        RAILS_STORAGE_PATH = "${home}/storage";
        RAILS_DATABASE_PATH = "${home}/production.sqlite";
        RAILS_CACHE_DATABASE_PATH = "${home}/production_cache.sqlite";
        RAILS_QUEUE_DATABASE_PATH = "${home}/production_queue.sqlite";
        RUBY_ENABLE_YJIT = "1";
        SOLID_QUEUE_IN_PUMA = "1";
      };
      exports = lib.concatStringsSep
        "\n"
        (lib.mapAttrsToList (name: value: "export ${name}=\"${value}\"") env);
      console = pkgs.writeShellScriptBin "entrance-exam-console" ''
        ${exports}
        export $(cat ${environmentFile} | xargs)
        cd ${serverPackage}
        ${gemsPackage}/bin/bundle exec rails c
      '';
      shell = pkgs.writeShellScriptBin "entrance-exam-shell" ''
        ${exports}
        export $(cat ${environmentFile} | xargs)
        export PATH="${gemsPackage}/bin/:$PATH"
        cd ${serverPackage}
        bash
      '';
    in
    {
      environment.systemPackages = [ console shell ];
      systemd.tmpfiles.rules = [
        "d /run/entrance-exam 0755 entrance-exam entrance-exam -"
        "d ${home}/storage 0755 entrance-exam entrance-exam -"
      ];
      systemd.services = {
        entrance-exam = {
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
          environment = env;
          path = [ gemsPackage gemsPackage.wrappedRuby ];
          serviceConfig = {
            EnvironmentFile = environmentFile;
            Type = "simple";
            User = "entrance-exam";
            Group = "entrance-exam";
            Restart = "on-failure";
            WorkingDirectory = serverPackage;
            ExecStartPre = [
              "${gemsPackage}/bin/bundle exec rails db:migrate"
            ];
            ExecStart = "${gemsPackage}/bin/puma -C ${serverPackage}/config/puma.rb";
          };
        };
      };
      users.users.entrance-exam = {
        group = "entrance-exam";
        home = home;
        createHome = true;
        uid = 696;
      };
      users.groups.entrance-exam.gid = 696;

      services.nginx = {
        virtualHosts."vanpetegem.gent" = {
          enableACME = true;
          forceSSL = true;
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

      security.doas.extraRules = [
        {
          users = [ "charlotte" ];
          noPass = true;
          cmd = "entrance-exam-console";
          runAs = "entrance-exam";
        }
        {
          users = [ "charlotte" ];
          noPass = true;
          cmd = "entrance-exam-shell";
          runAs = "entrance-exam";
        }
      ];

      age.secrets."passwords/services/entrance-exam" = {
        file = ../../../../secrets/passwords/services/entrance-exam.age;
        owner = "entrance-exam";
      };
    }
  );
}
