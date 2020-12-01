{ config, pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../configurations/eid.nix
    ../../profiles/bluetooth.nix
    ../../profiles/common.nix
    ../../profiles/graphical.nix
  ];

  networking = {
    hostId = "e718389d";
    hostName = "kharbranth";
  };

  time.timeZone = "Europe/Brussels";

  # Machine-specific application settings
  chvp = {
    stateVersion = "20.09";
    graphical = true;
    docker.enable = true;
    git.email = "charlotte.vanpetegem@ugent.be";
    zfs = {
      enable = true;
      encrypted = true;
      backups = [
        {
          path = "rpool/safe/data";
          remotePath = "zdata/recv/kharbranth/safe/data";
          fast = true;
          location = "lasting-integrity.vanpetegem.me";
        }
      ];
    };
  };
}
