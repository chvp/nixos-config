{ pkgs, lib, config, ... }:

{
  imports = [ ./hardware.nix ];

  boot.kernelPackages = pkgs.linuxPackages_6_18;

  networking.hostId = "6008fa3f";

  time.timeZone = "Europe/Brussels";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      bluetooth.enable = true;
      network.mobile = {
        enable = true;
        wireless-interface = "wlp192s0";
      };
      zfs = {
        enable = true;
        encrypted = true;
        backups = [
          {
            path = "rpool/safe/data";
            remotePath = "zdata/recv/kholinar/safe/data";
            fast = true;
            location = "elendel";
          }
        ];
        rootDataset = "rpool/local/root";
        rootPool = "rpool";
      };
    };
    development = {
      enable = true;
      android.enable = true;
    };
    games.enable = true;
    graphical.enable = true;
    programs = {
      calibre.enable = true;
      eid.enable = true;
      element.enable = true;
      hledger.enable = true;
      slack.enable = true;
      teams.enable = true;
      torrents.enable = true;
    };
  };
}
