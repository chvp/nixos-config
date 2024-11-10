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
        backups = [
          {
            path = "zroot/safe/data";
            remotePath = "zdata/recv/urithiru/safe/data";
            fast = true;
            location = "192.168.0.2";
          }
          {
            path = "zroot/safe/data";
            remotePath = "zdata/recv/urithiru/safe/data";
            fast = true;
            location = "192.168.0.2";
          }
          {
            path = "zdata/big-apps/accentor";
            remotePath = "zdata/recv/urithiru/big-apps/accentor";
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
        rootPool = "zroot";
      };
    };
    services = {
      accentor.enable = true;
      containers.externalInterface = "eno3";
      data-access.enable = true;
      torrents.enable = true;
    };
  };

  services.znapzend.zetup."zdata/data".destinations."socrates.machines.robbevp.be" = {
    plan = "1day=>1hour,1week=>1day,4week=>1week,1year=>1month,10year=>6month";
    host = "socrates.machines.robbevp.be";
    dataset = "zdata/data";
  };
}
