{ pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Brussels";

  networking.hostId = "10a4250f";

  chvp = {
    stateVersion = "24.11";
    base = {
      network = {
        ovh = {
          enable = true;
          publicInterface = "eno1";
          publicIPV4 = {
            ip = "162.19.60.238";
            gateway = "162.19.60.254";
          };
          publicIPV6 = {
            ip = "2001:41d0:203:cdee::";
            gateway = "2001:41d0:0203:cdff:00ff:00ff:00ff:00ff";
          };
          internalInterface = "eno2";
          internalIPV4 = "192.168.0.3";
        };
      };
      nix.enableDirenv = true;
      zfs = {
        enable = true;
        rootDataset = "zroot/local/root";
        rootPool = "zroot";
      };
    };
  };
}
