{ lib, pkgs, nixosConfigurations, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Berlin";

  networking.hostId = "079e60ba";

  chvp = {
    stateVersion = "20.09";
    base = {
      nix.enableDirenv = false;
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
        rootDataset = "zroot/local/root";
      };
    };
    games = {
      teeworlds.server = false;
      zeroad.server = true;
    };
    services = {
      accentor.enable = true;
      containers.externalInterface = "eno3";
      data-access.enable = true;
      deluge = {
        enable = true;
        count = 6;
      };
    };
  };

  services.telegraf.extraConfig.inputs.disk.mount_points = [ "/boot/ESP0" "/boot/ESP1" ];
}
