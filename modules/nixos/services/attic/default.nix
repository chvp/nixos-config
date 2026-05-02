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
              client_max_body_size 50M;
            '';
          };
        };
      }
    ];
    users = {
      users = {
        atticd = {
          home = "/var/lib/atticd";
          group = "atticd";
          isSystemUser = true;
          useDefaultShell = true;
        };
        nginx.extraGroups = [ "atticd" ];
      };
      groups.atticd = { };
    };
    services.atticd = {
      enable = true;
      environmentFile = config.age.secrets."passwords/services/atticd".path;
      settings = {
        listen = "[::]:8080";
        garbage-collection.default-retention-period = "2 months";
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
