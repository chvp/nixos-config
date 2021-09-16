{ config, lib, pkgs, ... }:

{
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
        path = [ pkgs.sshuttle pkgs.openssh pkgs.bash ];
        environment = { PASSWORD_FILE = config.age.secrets."passwords/ugent-vpn".path; };
        serviceConfig.ExecStart = config.age.secrets."files/programs/vpn/global".path;
      };
      ugent-local-vpn = {
        after = [ "network.target" ];
        conflicts = [ "ugent-global-vpn.service" ];
        path = [ pkgs.sshuttle pkgs.openssh pkgs.bash ];
        environment = { PASSWORD_FILE = config.age.secrets."passwords/ugent-vpn".path; };
        serviceConfig.ExecStart = config.age.secrets."files/programs/vpn/local".path;
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
    age.secrets."files/programs/vpn/local" = {
      file = ../../../secrets/files/programs/vpn/local.age;
      mode = "0500";
    };
    age.secrets."files/programs/vpn/global" = {
      file = ../../../secrets/files/programs/vpn/global.age;
      mode = "0500";
    };
  };
}
