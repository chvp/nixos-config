{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.chvp.services.git.enable = lib.mkOption {
    default = false;
    example = true;
  };

  imports = [
    ./docker-runner.nix
    ./native-runner.nix
  ];

  config = lib.mkIf config.chvp.services.git.enable {
    chvp.services.nginx.hosts = [
      {
        fqdn = "git.chvp.be";
        options = {
          locations."/" = {
            proxyPass = "http://unix:/run/forgejo/forgejo.socket";
            extraConfig = ''
              client_max_body_size 50M;
            '';
          };
        };
      }
    ];
    users = {
      users = {
        git = {
          home = "/var/lib/forgejo";
          group = "git";
          isSystemUser = true;
          useDefaultShell = true;
        };
        nginx.extraGroups = [ "git" ];
      };
      groups.git = { };
    };
    services = {
      forgejo = {
        enable = true;
        package = pkgs.forgejo;
        stateDir = "/var/lib/forgejo";
        user = "git";
        group = "git";
        database = {
          type = "postgres";
          user = "git";
          name = "git";
          createDatabase = true;
        };
        settings = {
          actions = {
            ARTIFACT_RETENTION_DAYS = 30;
            LOG_RETENTION_DAYS = 30;
          };
          admin = {
            SEND_NOTIFICATION_EMAIL_ON_NEW_USER = true;
          };
          repository = {
            DISABLE_DOWNLOAD_SOURCE_ARCHIVES = true;
            ENABLE_PUSH_CREATE_USER = true;
            ENABLE_PUSH_CREATE_ORG = true;
          };
          server = {
            DOMAIN = "git.chvp.be";
            PROTOCOL = "http+unix";
            ROOT_URL = "https://git.chvp.be/";
            HTTP_ADDR = "/run/forgejo/forgejo.socket";
          };
          service = {
            ENABLE_NOTIFY_MAIL = true;
            REGISTER_MANUAL_CONFIRM = true;
          };
          mailer = {
            ENABLED = true;
            PROTOCOL = "smtps";
            SMTP_ADDR = "mail.vanpetegem.me";
            SMPT_PORT = 465;
            USER = "git_chvp_sa";
            FROM = "Git <git@chvp.be>";
          };
          "email.incoming" = {
            ENABLED = true;
            REPLY_TO_ADDRESS = "git+%{token}@chvp.be";
            HOST = "mail.vanpetegem.me";
            PORT = 993;
            USERNAME = "git_chvp_sa";
            USE_TLS = true;
          };
          session = {
            COOKIE_SECURE = true;
            PROVIDER = "db";
            COOKIE_NAME = "forgejo_session";
          };
          log = {
            ROOT_PATH = "/var/log/forgejo";
          };
        };
        secrets = {
          mailer.PASSWD = config.age.secrets."passwords/services/git/mail-password".path;
          "email.incoming".PASSWORD = config.age.secrets."passwords/services/git/mail-password".path;
        };
      };
    };
    age.secrets = {
      "passwords/services/git/mail-password" = {
        file = ../../../secrets/passwords/services/git/mail-password.age;
        owner = "git";
      };
    };
  };
}
