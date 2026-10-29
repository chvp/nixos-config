{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.chvp.services.nextcloud.enable = lib.mkOption {
    default = false;
    example = true;
  };
  config = lib.mkIf config.chvp.services.nextcloud.enable {
    chvp = {
      base.zfs.systemLinks = [
        {
          path = "/var/lib/redis-nextcloud";
          type = "cache";
        }
      ];
      services.nginx.hosts = [
        {
          fqdn = "collabora.vanpetegem.me";
          basicProxy = "http://[::1]:9980";
          extraProxyOptions.proxyWebsockets = true;
        }
      ];
    };
    networking.hosts = {
      "127.0.0.1" = [
        "nextcloud.vanpetegem.me"
        "collabora.vanpetegem.me"
      ];
      "::1" = [
        "nextcloud.vanpetegem.me"
        "collabora.vanpetegem.me"
      ];
    };
    services = {
      collabora-online = {
        enable = true;
        port = 9980;
        settings = {
          ssl = {
            enable = false;
            termination = true;
          };
          net = {
            listen = "loopback";
            post_allow.host = [ "::1" ];
          };
          storage.wopi = {
            "@allow" = true;
            host = [ "nextcloud.vanpetegem.me" ];
          };
          server_name = "collabora.vanpetegem.me";
        };
      };
      nextcloud = {
        enable = true;
        home = "/var/lib/nextcloud";
        https = true;
        hostName = "nextcloud.vanpetegem.me";
        autoUpdateApps.enable = true;
        package = pkgs.nextcloud35;
        caching.redis = true;
        configureRedis = true;
        config.dbtype = "pgsql";
        database.createLocally = true;
        settings = {
          mail_smtpmode = "sendmail";
          mail_domain = "vanpetegem.me";
          user_oidc = {
            single_logout = false;
            disable_account_creation = true;
          };
        };
      };
      nginx.virtualHosts."nextcloud.vanpetegem.me" = {
        forceSSL = true;
        useACMEHost = "vanpetegem.me";
        extraConfig = ''
          fastcgi_connect_timeout 10m;
          fastcgi_read_timeout 10m;
          fastcgi_send_timeout 10m;
        '';
      };
    };
  };
}
