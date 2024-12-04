{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Brussels";

  networking = {
    hostId = "338495bc";
    useDHCP = false;
  };

  systemd.network = {
    enable = true;
    networks."enp7s0" = {
      enable = true;
      matchConfig = { Name = "enp7s0"; };
      address = [
        "37.27.113.55/26"
        "2a01:4f9:3070:2382::/64"
      ];
      gateway = [ "37.27.113.1" ];
      routes = [
        {
          Gateway = "fe80::1";
          GatewayOnLink = true;
        }
      ];
      dns = [
        "1.1.1.1"
        "1.0.0.1"
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];
    };
  };

  chvp = {
    stateVersion = "24.11";
    base = {
      nix.enableDirenv = true;
      zfs = {
        enable = true;
        backups = [
          {
            path = "zroot/safe/data";
            remotePath = "zroot/recv/elendel/safe/data";
            fast = true;
            location = "marabethia.vanpetegem.me";
          }
          {
            path = "zroot/safe/services/transmission";
            remotePath = "zroot/recv/elendel/safe/services/transmission";
            fast = true;
            location = "marabethia.vanpetegem.me";
          }
          {
            path = "zdata/safe/data";
            remotePath = "zdata/data";
            fast = false;
            location = "socrates.machines.robbevp.be";
          }
        ];
        rootDataset = "zroot/local/root";
        rootPool = "zroot";
      };
    };
    services = {
      accentor.enable = true;
      containers.externalInterface = "enp7s0";
      data-access.enable = true;
      git.runner.enable = true;
      torrents.enable = true;
    };
  };
  services.postgresql.dataDir = lib.mkForce "/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
}
