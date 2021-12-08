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
        autoUpdateApps.enable = true;
        package = pkgs.nextcloud23;
        config = {
          dbuser = "nextcloud";
          dbname = "nextcloud";
          dbtype = "pgsql";
          dbhost = "/run/postgresql";
          adminuser = "admin";
          adminpassFile = config.age.secrets."passwords/services/nextcloud-admin".path;
        };
      };
      nginx.virtualHosts."nextcloud.vanpetegem.me" = {
        forceSSL = true;
        useACMEHost = "vanpetegem.me";
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
