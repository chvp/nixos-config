{ config, lib, pkgs, ... }:

{
  options.chvp.services.nextcloud.enable = lib.mkOption {
    default = false;
    example = true;
  };
  config = lib.mkIf config.chvp.services.nextcloud.enable {
    chvp.base.zfs.systemLinks = [
      { path = "/var/lib/redis-nextcloud"; type = "cache"; }
    ];
    services = {
      nextcloud = {
        home = "/var/lib/nextcloud";
        https = true;
        hostName = "nextcloud.vanpetegem.me";
        enable = true;
        autoUpdateApps.enable = true;
        package = pkgs.nextcloud33;
        caching.redis = true;
        configureRedis = true;
        config = {
          dbtype = "pgsql";
          adminuser = "admin";
          adminpassFile = config.age.secrets."passwords/services/nextcloud-admin".path;
        };
        database.createLocally = true;
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
    age.secrets."passwords/services/nextcloud-admin" = {
      file = ../../../../secrets/passwords/services/nextcloud-admin.age;
      owner = "nextcloud";
    };
  };
}
