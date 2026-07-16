{ config, lib, pkgs, ... }:

{
  options.chvp.services.attic.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.attic.enable {
    chvp.services.nginx.hosts = [
      {
        fqdn = "attic.chvp.be";
        options = {
          locations."/" = {
            proxyPass = "http://localhost:8080";
            extraConfig = ''
              client_max_body_size 5G;
            '';
          };
        };
      }
    ];
    users = {
      users = {
        atticd = {
          home = "/var/lib/attic";
          group = "atticd";
          isSystemUser = true;
          useDefaultShell = true;
        };
        nginx.extraGroups = [ "atticd" ];
      };
      groups.atticd = { };
    };
    services = {
      atticd = {
        enable = true;
        environmentFile = config.age.secrets."passwords/services/atticd".path;
        settings = {
          database.url = "postgresql://atticd?host=/run/postgresql&user=atticd";
          listen = "[::]:8080";
          garbage-collection.default-retention-period = "1 month";
          chunking = {
            nar-size-threshold = 0;
            min-size = 65536;
            avg-size = 131072;
            max-size = 262144;
          };
          storage = {
            type = "local";
            path = "/var/lib/attic/storage";
          };
        };
      };
      postgresql = {
        ensureUsers = [{
          name = "atticd";
          ensureDBOwnership = true;
        }];
        ensureDatabases = [ "atticd" ];
      };
    };
    age.secrets = {
      "passwords/services/atticd" = {
        file = ../../../../secrets/passwords/services/atticd.age;
        owner = "atticd";
      };
    };
  };
}
