{ pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Berlin";

  networking.hostId = "b352adfe";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      network.ovh = {
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
      nix.enableDirenv = false;
      zfs = {
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
            path = "zdata/big-apps/mail";
            remotePath = "zdata/recv/lasting-integrity/big-apps/mail";
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
      garmin-scraper.enable = false;
      grafana.enable = true;
      mail.enable = true;
      matrix.enable = true;
      nginx.hosts = [
        {
          fqdn = "vanpetegem.me";
          options = {
            locations = let matrixRedirect = {
              proxyPass = "http://127.0.0.1:8448";
              extraConfig = ''
                proxy_read_timeout 600;
                client_max_body_size 10M;
                proxy_set_header X-Forwarded-Ssl on;
              '';
            }; in
              {
                "/_matrix" = matrixRedirect;
                "/.well-known/matrix" = {
                  root = pkgs.runCommandNoCC "well-known-matrix" { } ''
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
          options.locations."/".return = "307 https://www.cvpetegem.be$request_uri";
        }
        { fqdn = "www.cvpetegem.be"; }
        {
          fqdn = "chvp.be";
          options.locations."/".return = "307 https://www.chvp.be$request_uri";
        }
        { fqdn = "www.chvp.be"; }
      ];
      nextcloud.enable = true;
      syncthing.enable = true;
    };
  };
  programs.msmtp.enable = false;
  services.telegraf.extraConfig.inputs.disk.mount_points = [ "/boot/ESP0" "/boot/ESP1" ];
}
