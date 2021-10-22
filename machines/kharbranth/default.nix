{ config, pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  networking.hostId = "e718389d";

  time.timeZone = "Europe/Brussels";

  environment.etc."jetbrains/python".source = pkgs.python3;
  environment.systemPackages = [ pkgs.jetbrains.pycharm-community ];

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      bluetooth.enable = true;
      network.mobile = {
        enable = true;
        wireless-interface = "wlp2s0";
        wired-interfaces = {
          "enp0s20f0u1u2" = { macAddress = "10:65:30:df:80:f5"; };
          "enp0s31f6" = { macAddress = "10:65:30:df:80:f5"; };
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
}
