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
        wireguard.server = true;
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
            path = "zroot/safe/services/dkim";
            remotePath = "zdata/recv/marabethia/safe/services/dkim";
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
            path = "zroot/safe/services/mail";
            remotePath = "zdata/recv/marabethia/safe/services/mail";
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
          {
            path = "zroot/safe/services/sieve";
            remotePath = "zdata/recv/marabethia/safe/services/sieve";
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
      mail.enable = true;
      matrix.enable = true;
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
        {
          fqdn = "vanpetegem.me";
          options = {
            locations = {
              "/_matrix" = {
                proxyPass = "http://127.0.0.1:8448";
                extraConfig = ''
                  proxy_read_timeout 600;
                  client_max_body_size 10M;
                  proxy_set_header X-Forwarded-Ssl on;
                '';
              };
              "/.well-known/matrix" = {
                root = pkgs.runCommand "well-known-matrix" { } ''
                  mkdir -p $out/.well-known/matrix
                  echo '{"m.server":"matrix.vanpetegem.me:443"}' > $out/.well-known/matrix/server
                  echo '{"m.homeserver":{"base_url":"https://matrix.vanpetegem.me"}}' > $out/.well-known/matrix/client
                '';
                extraConfig = ''
                  default_type application/json;
                  add_header 'access-control-allow-origin' '*' always;
                  add_header 'access-control-allow-methods' 'GET, HEAD, POST, PUT, DELETE, OPTIONS' always;
                  add_header 'access-control-allow-headers' 'X-Requested-With, Content-Type, Authorization, Date' always;
                '';
                priority = 1;
              };
              "/".return = "307 https://www.vanpetegem.me$request_uri";
            };
          };
        }
        { fqdn = "www.vanpetegem.me"; }
      ];
    };
  };
  programs.msmtp.enable = false;
  services.postgresql.dataDir = lib.mkForce "/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
}
