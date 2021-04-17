{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./secret.nix
  ];

  time.timeZone = "Europe/Berlin";

  networking.hostId = "b352adfe";

  chvp = {
    stateVersion = "20.09";
    docker.enable = true;
    nginx.enable = true;
    nextcloud.enable = true;
    ovh.enable = true;
    smartd.enable = true;
    sshd.enable = true;
    syncthing-server.enable = true;
    tetris.enable = true;
    zfs = {
      enable = true;
      backups = [{
        path = "zroot/safe/data";
        remotePath = "zdata/recv/lasting-integrity/safe/data";
        fast = true;
        location = "192.168.0.1";
      }];
      rootDataset = "zroot/local/root";
    };
  };
}
