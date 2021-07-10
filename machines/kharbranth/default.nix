{ config, pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  networking.hostId = "e718389d";

  time.timeZone = "Europe/Brussels";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      bluetooth.enable = true;
      network.networkmanager.enable = true;
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
      hledger.enable = true;
      obs.enable = true;
    };
    work.enable = true;
  };
}
