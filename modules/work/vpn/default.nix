{ config, lib, pkgs, ... }:

{
  imports = [
    ./secret.nix
  ];

  options = {
    chvp.work.vpn.enable = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf config.chvp.work.vpn.enable {
    systemd.services = {
      ugent-global-vpn = {
        after = [ "network.target" ];
        conflicts = [ "ugent-local-vpn.service" ];
      };
      ugent-local-vpn = {
        after = [ "network.target" ];
        conflicts = [ "ugent-global-vpn.service" ];
      };
    };
    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
          if (action.id == "org.freedesktop.systemd1.manage-units" && action.lookup("unit") == "ugent-global-vpn.service") {
              return polkit.Result.YES;
          }
          if (action.id == "org.freedesktop.systemd1.manage-units" && action.lookup("unit") == "ugent-local-vpn.service") {
              return polkit.Result.YES;
          }
      });
    '';
    age.secrets."passwords/ugent-vpn".file = ../../../secrets/passwords/ugent-vpn.age;
  };
}
