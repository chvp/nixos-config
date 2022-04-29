{ config, lib, pkgs, ... }:

{
  options.chvp.services.grafana.enable = lib.mkEnableOption "grafana";

  config = lib.mkIf config.chvp.services.grafana.enable {
    chvp.services.nginx.hosts = [{
      fqdn = "stats.chvp.be";
      options.locations."/" = {
        proxyPass = "http://grafana";
        proxyWebsockets = true;
      };
    }];
    users.users = {
      influxdb2.extraGroups = [ "acme" ];
      nginx.extraGroups = [ "grafana" ];
    };
    networking.firewall.allowedTCPPorts = [ 8086 ];
    services = {
      nginx.upstreams.grafana.servers = { "unix:/run/grafana/grafana.sock" = {}; };
      influxdb2 = {
        enable = true;
        settings = {
          reporting-disabled = true;
          tls-cert = "${config.security.acme.certs."vanpetegem.me".directory}/fullchain.pem";
          tls-key = "${config.security.acme.certs."vanpetegem.me".directory}/key.pem";
        };
      };
      grafana = {
        enable = true;
        analytics.reporting.enable = false;
        port = 3000;
        domain = "stats.chvp.be";
        rootUrl = "https://stats.chvp.be/";
        dataDir = "${config.chvp.dataPrefix}/var/lib/grafana";
        protocol = "socket";
        auth.anonymous = {
          enable = true;
          org_name = "Van Petegem";
        };
        smtp = {
          enable = true;
          user = "noreply@vanpetegem.me";
          fromAddress = "noreply@vanpetegem.me";
          passwordFile = config.age.secrets."passwords/services/grafana/smtp".path;
        };
        database = {
          user = "grafana";
          type = "postgres";
          host = "/run/postgresql/";
          name = "grafana";
        };
        users = {
          allowSignUp = false;
        };
        security = {
          adminUser = "chvp";
          adminPasswordFile = config.age.secrets."passwords/services/grafana/admin-password".path;
          secretKeyFile = config.age.secrets."passwords/services/grafana/secret-key".path;
        };
        extraOptions = {
          USERS_DEFAULT_THEME = "light";
        };
      };
      postgresql = {
        enable = true;
        dataDir = "${config.chvp.dataPrefix}/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
        ensureDatabases = [ "grafana" ];
        ensureUsers = [{
          name = "grafana";
          ensurePermissions = { "DATABASE grafana" = "ALL PRIVILEGES"; };
        }];
      };
    };
    age.secrets."passwords/services/grafana/smtp" = {
      file = ../../../secrets/passwords/services/grafana/smtp.age;
      owner = "grafana";
    };
    age.secrets."passwords/services/grafana/admin-password" = {
      file = ../../../secrets/passwords/services/grafana/admin-password.age;
      owner = "grafana";
    };
    age.secrets."passwords/services/grafana/secret-key" = {
      file = ../../../secrets/passwords/services/grafana/secret-key.age;
      owner = "grafana";
    };
  };
}
