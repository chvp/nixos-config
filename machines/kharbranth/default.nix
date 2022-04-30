{ config, pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  networking.hostId = "7a62a099";

  time.timeZone = "Europe/Brussels";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      bluetooth.enable = true;
      network.mobile = {
        enable = true;
        wireless-interface = "wlp0s20f3";
        wired-interfaces = {
          "enp0s13f0u2u2" = { };
        };
      };
      zfs = {
        encrypted = true;
        backups = [
          {
            path = "rpool/safe/data";
            remotePath = "zdata/recv/kharbranth/safe/data";
            fast = true;
            location = "lasting-integrity";
          }
        ];
        rootDataset = "rpool/local/root";
      };
    };
    development = {
      enable = true;
      android.enable = true;
      git.email = "charlotte.vanpetegem@ugent.be";
    };
    graphical.enable = true;
    programs = {
      eid.enable = true;
      element.enable = true;
      hledger.enable = true;
      obs.enable = true;
    };
    work.enable = true;
  };

  services.telegraf.extraConfig.inputs.disk.mount_points = [ "/" "/boot" ];
}
