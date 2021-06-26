{ config, pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  networking.hostId = "e718389d";

  time.timeZone = "Europe/Brussels";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    graphical = true;
    bluetooth.enable = true;
    git.email = "charlotte.vanpetegem@ugent.be";
    zfs = {
      enable = true;
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
}
