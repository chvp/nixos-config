{
  config,
  lib,
  pkgs,
  ...
}:

let
  username = config.chvp.username;
in
{
  options.chvp.services.accentor.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.accentor.enable {
    services.postgresql.enable = true;

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

    security.sudo.extraRules = [
      {
        users = [ username ];
        runAs = "accentor";
        commands = [
          {
            command = "/run/current-system/sw/bin/accentor-console";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];

    age.secrets."passwords/services/accentor" = {
      file = ../../../secrets/passwords/services/accentor.age;
      owner = "accentor";
    };
  };
}
