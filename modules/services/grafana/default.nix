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
      nginx.upstreams.grafana.servers = { "unix:/run/grafana/grafana.sock" = { }; };
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
        dataDir = "${config.chvp.dataPrefix}/var/lib/grafana";
        settings = {
          analytics.reporting_enabled = false;
          "auth.anonymous" = {
            enabled = "true";
            org_name = "Van Petegem";
          };
          database = {
            user = "grafana";
            type = "postgres";
            host = "/run/postgresql/";
            name = "grafana";
          };
          security = {
            admin_user = "chvp";
            admin_password = "$__file{${config.age.secrets."passwords/services/grafana/admin-password".path}}";
            secret_key = "$__file{${config.age.secrets."passwords/services/grafana/secret-key".path}}";
          };
          server = {
            domain = "stats.chvp.be";
            http_port = 3000;
            protocol = "socket";
            root_url = "https://stats.chvp.be";
            socket = "/run/grafana/grafana.sock";
          };
          smtp = {
            enabled = true;
            host = "mail.vanpetegem.me:25";
            user = "noreply@vanpetegem.me";
            from_address = "noreply@vanpetegem.me";
            password = "$__file{${config.age.secrets."passwords/services/grafana/smtp".path}}";
          };
          users = {
            default_theme = "light";
            allow_sign_up = false;
          };
        };
      };
      grafana-image-renderer = {
        enable = true;
        provisionGrafana = true;
        chromium = pkgs.ungoogled-chromium;
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
