{ lib, pkgs, nixosConfigurations, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Berlin";

  networking.hostId = "079e60ba";

  chvp = {
    stateVersion = "20.09";
    base = {
      nix = {
        enableDirenv = false;
        slowGc = true;
      };
      network.ovh = {
        enable = true;
        publicIPV4 = {
          ip = "193.70.44.178";
          gateway = "193.70.44.254";
        };
        publicIPV6 = {
          ip = "2001:41d0:0303:0ab2::";
          gateway = "2001:41d0:0303:0aff:ff:ff:ff:ff";
        };
        internalIPV4 = "192.168.0.1";
      };
      zfs = {
        enable = true;
        backups = [ ];
        rootDataset = "zroot/local/root";
        rootPool = "zroot";
      };
    };
  };
}
