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
    chvp.base.zfs.systemLinks = [
      {
        path = "/var/lib/redis-nextcloud";
        type = "cache";
      }
    ];
    services = {
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
