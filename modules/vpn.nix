{ config, lib, pkgs, ... }:

{
  imports = [
    ./vpn/secret.nix
  ];

  options = {
    chvp.vpn.ugent.enable = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf config.chvp.vpn.ugent.enable {
    systemd.services = {
      ugent-global-vpn.after = [ "network.target" ];
      ugent-local-vpn.after = [ "network.target" ];
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
    age.secrets."passwords/ugent-vpn".file = ../secrets/passwords/ugent-vpn.age;
  };
}
