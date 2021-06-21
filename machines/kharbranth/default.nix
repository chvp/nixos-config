{ config, pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/graphical.nix
  ];

  networking.hostId = "e718389d";

  time.timeZone = "Europe/Brussels";

  # Machine-specific application settings
  chvp = {
    stateVersion = "20.09";
    graphical = true;
    bluetooth.enable = true;
    docker.enable = true;
    eid.enable = true;
    git.email = "charlotte.vanpetegem@ugent.be";
    sshd.enable = true;
    vpn.ugent.enable = true;
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
    zotero.enable = true;
  };
}
