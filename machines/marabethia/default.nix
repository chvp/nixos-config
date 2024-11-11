{ lib, pkgs, config, ... }:

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
        backups = [
          {
            path = "zroot/safe/data";
            remotePath = "zdata/recv/marabethia/safe/data";
            fast = true;
            location = "elendel.vanpetegem.me";
          }
          {
            path = "zroot/safe/services/forgejo";
            remotePath = "zdata/recv/marabethia/safe/services/forgejo";
            fast = true;
            location = "elendel.vanpetegem.me";
          }
          {
            path = "zroot/safe/services/nextcloud";
            remotePath = "zdata/recv/marabethia/safe/services/nextcloud";
            fast = true;
            location = "elendel.vanpetegem.me";
          }
          {
            path = "zroot/safe/services/postgresql";
            remotePath = "zdata/recv/marabethia/safe/services/postgresql";
            fast = true;
            location = "elendel.vanpetegem.me";
          }
        ];
        rootDataset = "zroot/local/root";
        rootPool = "zroot";
      };
    };
    games = {
      particles.server = true;
      tetris.server = true;
    };
    services = {
      git.enable = true;
      nextcloud.enable = true;
      nginx.hosts = [
        {
          fqdn = "cvpetegem.be";
          options.locations."/".return = "307 https://www.chvp.be$request_uri";
        }
        {
          fqdn = "www.cvpetegem.be";
          options.locations."/".return = "307 https://www.chvp.be$request_uri";
        }
        {
          fqdn = "chvp.be";
          options.locations = {
            "/".return = "307 https://www.chvp.be$request_uri";
          };
        }
        {
          fqdn = "www.chvp.be";
          options.root = pkgs."www.chvp.be";
        }
        {
          fqdn = "vanpetegem.be";
          options.locations."/".return = "307 https://www.vanpetegem.be$request_uri";
        }
        { fqdn = "www.vanpetegem.be"; }
      ];
    };
  };
  services.postgresql.dataDir = lib.mkForce "/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
}
