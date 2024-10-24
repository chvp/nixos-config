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
        wireguard.server = true;
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
          {
            path = "zdata/big-apps/influxdb2";
            remotePath = "zdata/recv/lasting-integrity/big-apps/influxdb2";
            fast = true;
            location = "192.168.0.1";
          }
          {
            path = "zdata/big-apps/git";
            remotePath = "zdata/recv/lasting-integrity/big-apps/git";
            fast = true;
            location = "192.168.0.1";
          }
          {
            path = "zdata/big-apps/mail";
            remotePath = "zdata/recv/lasting-integrity/big-apps/mail";
            fast = true;
            location = "192.168.0.1";
          }
          {
            path = "zdata/big-apps/mastodon";
            remotePath = "zdata/recv/lasting-integrity/big-apps/mastodon";
            fast = true;
            location = "192.168.0.1";
          }
          {
            path = "zdata/big-apps/nextcloud";
            remotePath = "zdata/recv/lasting-integrity/big-apps/nextcloud";
            fast = true;
            location = "192.168.0.1";
          }
        ];
        rootDataset = "zroot/local/root";
      };
    };
    games = {
      particles.server = true;
      tetris.server = true;
    };
    services = {
      garmin-scraper.enable = true;
      git.enable = true;
      grafana.enable = true;
      mail.enable = true;
      mastodon.enable = true;
      matrix.enable = true;
      nginx.hosts = [
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
            "/phd/register".return = "307 https://nextcloud.vanpetegem.me/apps/forms/s/PrWYKWkwgwA7naE3ryRwmfcd";
          };
        }
        {
          fqdn = "www.chvp.be";
          options.root = pkgs."www.chvp.be";
        }
      ];
      nextcloud.enable = true;
    };
  };
  programs.msmtp.enable = false;
  services.telegraf.extraConfig.inputs.disk.mount_points = [ "/boot/ESP0" "/boot/ESP1" ];
}
