{ config, lib, pkgs, ... }:

{
  options.chvp.services.headscale.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.headscale.enable {
    networking.firewall = {
      allowedTCPPorts = [ 50443 ];
      allowedUDPPorts = [ 3478 ];
    };
    services = {
      headscale = {
        enable = true;
        serverUrl = "https://headscale.vanpetegem.me";
        privateKeyFile = config.age.secrets."passwords/services/headscale".path;
        database = {
          type = "postgres";
          name = "headscale";
          user = "headscale";
          host = "/run/postgresql";
        };
        dns = {
          domains = [ "vanpetegem.internal" ];
          baseDomain = "vanpetegem.me";
        };
      };
      postgresql = {
        enable = true;
        dataDir = "${config.chvp.dataPrefix}/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
        ensureDatabases = [ "headscale" ];
        ensureUsers = [{
          name = "headscale";
          ensurePermissions = { "DATABASE headscale" = "ALL PRIVILEGES"; };
        }];
      };
    };
    chvp.services.nginx.hosts = [
      {
        fqdn = "headscale.vanpetegem.me";
        options.locations."/" = {
          proxyPass = "http://localhost:8080";
          extraConfig = ''
            proxy_buffering off;
            proxy_set_header X-Forwarded-Ssl on;
          '';
          proxyWebsockets = true;
        };
      }
    ];
    age.secrets."passwords/services/headscale" = {
      file = ../../../secrets/passwords/services/headscale.age;
      owner = "headscale";
    };
  };
}
