{ config, lib, pkgs, ... }:

{
  options.chvp.nextcloud.enable = lib.mkOption {
    default = false;
    example = true;
  };
  config = lib.mkIf config.chvp.nextcloud.enable {
    services = {
      nextcloud = {
        home = "${config.chvp.dataPrefix}/var/lib/nextcloud";
        https = true;
        hostName = "nextcloud.vanpetegem.me";
        enable = true;
        autoUpdateApps.enable = true;
        package = pkgs.nextcloud21;
        config = {
          dbuser = "nextcloud";
          dbname = "nextcloud";
          dbtype = "pgsql";
          dbhost = "/run/postgresql";
          adminuser = "admin";
          adminpassFile = "${config.chvp.dataPrefix}/var/secrets/nextcloud-admin-password";
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
  };
}
