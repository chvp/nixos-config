{ pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Berlin";

  networking.hostId = "b352adfe";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      network = {
        ovh = {
          enable = true;
          publicIPV4 = {
            ip = "54.38.222.69";
            gateway = "54.38.222.254";
          };
          publicIPV6 = {
            ip = "2001:41d0:0700:1445::";
            gateway = "2001:41d0:0700:14ff:ff:ff:ff:ff";
          };
          internalIPV4 = "192.168.0.2";
        };
      };
      nix.enableDirenv = false;
      zfs = {
        enable = true;
        backups = [
          {
            path = "zroot/safe/data";
            remotePath = "zdata/recv/lasting-integrity/safe/data";
            fast = true;
            location = "192.168.0.1";
          }
        ];
        rootDataset = "zroot/local/root";
        rootPool = "zroot";
      };
    };
  };
}
