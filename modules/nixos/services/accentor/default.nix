{ config, lib, pkgs, ... }:

{
  options.chvp.services.accentor.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.accentor.enable {
    services.postgresql.enable = true;
    chvp.services.nginx.hosts = [{ fqdn = "dummy.vanpetegem.me"; basicProxy = "http://localhost:3000"; }];

    services.accentor = {
      enable = true;
      home = "/var/lib/accentor";
      hostname = "accentor.vanpetegem.me";
      environmentFile = config.age.secrets."passwords/services/accentor".path;
      rescanTimer = {
        enable = true;
        dates = "00:00";
      };
      nginx = {
        forceSSL = true;
        useACMEHost = "vanpetegem.me";
      };
    };

    security.doas.extraRules = [{
      users = [ "charlotte" ];
      noPass = true;
      cmd = "accentor-console";
      runAs = "accentor";
    }];

    age.secrets."passwords/services/accentor" = {
      file = ../../../../secrets/passwords/services/accentor.age;
      owner = "accentor";
    };
  };
}
