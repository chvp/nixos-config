{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./secret.nix
  ];

  time.timeZone = "Europe/Berlin";

  networking.hostId = "b352adfe";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      network.ovh.enable = true;
      zfs = {
        backups = [{
          path = "zroot/safe/data";
          remotePath = "zdata/recv/lasting-integrity/safe/data";
          fast = true;
          location = "192.168.0.1";
        }];
        rootDataset = "zroot/local/root";
      };
    };
    development.enable = true;
    games.tetris.server = true;
    services = {
      nextcloud.enable = true;
      syncthing.enable = true;
    };
  };
}
