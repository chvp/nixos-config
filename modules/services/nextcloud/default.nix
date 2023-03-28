{ config, lib, pkgs, ... }:

{
  options.chvp.services.nextcloud.enable = lib.mkOption {
    default = false;
    example = true;
  };
  config = lib.mkIf config.chvp.services.nextcloud.enable {
    services = {
      nextcloud = {
        home = "${config.chvp.dataPrefix}/var/lib/nextcloud";
        https = true;
        hostName = "nextcloud.vanpetegem.me";
        enable = true;
        enableBrokenCiphersForSSE = false;
        autoUpdateApps.enable = true;
        package = pkgs.nextcloud26;
        caching.redis = true;
        config = {
          dbuser = "nextcloud";
          dbname = "nextcloud";
          dbtype = "pgsql";
          dbhost = "/run/postgresql";
          adminuser = "admin";
          adminpassFile = config.age.secrets."passwords/services/nextcloud-admin".path;
        };
        extraOptions = {
          redis = {
            host = "127.0.0.1";
            port = 31638;
            dbindex = 0;
            timeout = 1.5;
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
      postgresql = {
        enable = true;
        dataDir = "${config.chvp.dataPrefix}/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
        ensureDatabases = [ "nextcloud" ];
        ensureUsers = [{
          name = "nextcloud";
          ensurePermissions = { "DATABASE nextcloud" = "ALL PRIVILEGES"; };
        }];
      };
      redis.servers.nextcloud = {
        enable = true;
        port = 31638;
        bind = "127.0.0.1";
      };
    };
    age.secrets."passwords/services/nextcloud-admin" = {
      file = ../../../secrets/passwords/services/nextcloud-admin.age;
      owner = "nextcloud";
    };
    systemd.services."nextcloud-setup" = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };
    users.users.nextcloud.uid = 996;
    users.groups.nextcloud.gid = 996;
  };
}
