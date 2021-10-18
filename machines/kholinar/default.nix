{ pkgs, lib, ... }:

{
  imports = [ ./hardware.nix ];

  networking.hostId = "3cc1a4b2";

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
            remotePath = "zdata/recv/kholinar/safe/data";
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
    };
    games.enable = true;
    graphical.enable = true;
    programs = {
      calibre.enable = false;
      deluge.enable = true;
      eid.enable = true;
      element.enable = true;
      hledger.enable = true;
      obs.enable = true;
    };
    # It's a pandemic.
    work.enable = true;
  };
}
