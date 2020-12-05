{ pkgs, lib, ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/graphical.nix
  ];

  networking = {
    hostId = "3cc1a4b2";
    hostName = "kholinar";
  };

  time.timeZone = "Europe/Brussels";

  # Machine-specific settings
  chvp = {
    stateVersion = "20.09";
    graphical = true;
    bluetooth.enable = true;
    docker.enable = true;
    eid.enable = true;
    git.email = "charlotte@vanpetegem.me";
    nix.enableUnfree = true;
    zfs = {
      enable = true;
      encrypted = true;
      backups = [
        {
          path = "rpool/safe/data";
          remotePath = "zdata/recv/kholinar/safe/data";
          fast = true;
          location = "lasting-integrity";
        }
      ];
    };
  };
}
