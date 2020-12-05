{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./secret.nix
  ];

  time.timeZone = "Europe/Berlin";

  networking = {
    hostName = "urithiru";
    hostId = "079e60ba";
  };

  chvp = {
    stateVersion = "20.09";
    docker.enable = true;
    nginx.enable = true;
    ovh.enable = true;
    smartd.enable = true;
    sshd.enable = true;
    zfs = {
      enable = true;
      backups = [
        {
          path = "zroot/safe/data";
          remotePath = "zdata/recv/urithiru/safe/data";
          fast = true;
          location = "192.168.0.2";
        }
        {
          path = "zdata/data";
          remotePath = "zdata/data";
          fast = false;
          location = "192.168.0.2";
        }
      ];
    };
  };
}
