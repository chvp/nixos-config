{ config, lib, pkgs, ... }:

{
  options.chvp.services.git.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.git.enable {
    chvp.services.nginx.hosts = [{
      fqdn = "git.chvp.be";
      options = {
        root = pkgs.gitea.data;
        locations = {
          "/".tryFiles = "$uri @proxy";
          "@proxy" = {
            proxyPass = "http://unix:/run/gitea/gitea.sock";
            proxyWebsockets = true;
          };
        };
      };
    }];
    users = {
      users = {
        git = {
          uid = 963;
          home = "/var/lib/git";
          group = "git";
          isSystemUser = true;
          useDefaultShell = true;
        };
        nginx.extraGroups = [ "git" ];
      };
      groups.git.gid = 963;
    };
    services.openssh.settings.AcceptEnv = "GIT_PROTOCOL";
    services.gitea = {
      enable = true;
      stateDir = "/var/lib/git";
      user = "git";
      database = {
        type = "postgres";
        createDatabase = true;
        user = "git";
        name = "git";
      };
      dump.enable = true;
      lfs.enable = true;
      appName = "Charlotte's personal git server";
      domain = "git.chvp.be";
      rootUrl = "https://git.chvp.be/";
      enableUnixSocket = true;
      settings = {
        repository = {
          DEFAULT_PRIVATE = "private";
          ENABLE_PUSH_CREATE_USER = true;
          ENABLE_PUSH_CREATE_ORG = true;
        };
        "repository.pull-request".DEFAULT_MERGE_STYLE = "squash";
        "repository.mimetype_mapping" = {
          ".apk" = "application/vnd.android.package-archive";
        };
        ui.DEFAULT_SHOW_FULL_NAME = true;
        security.DISABLE_GIT_HOOKS = false;
        service = {
          ENABLE_NOTIFY_EMAIL = true;
          EMAIL_DOMAIN_WHITELIST = "chvp.be";
          REGISTER_EMAIL_CONFIRM = true;
          AUTO_WATCH_ON_CHANGES = true;
        };
        mailer = {
          ENABLED = true;
          FROM = "git@chvp.be";
          PROTOCOL = "smtp";
          SMTP_ADDR = "localhost";
          SMTP_PORT = 25;
        };
        session.COOKIE_SECURE = true;
        cron = {
          ENABLED = true;
          SCHEDULE = "@every 1h";
        };
      };
    };
  };
}
