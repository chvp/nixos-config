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
        rootPool = "zroot";
      };
    };
    services = {
      mail.enable = true;
      matrix.enable = true;
      nginx.hosts = [
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
      nextcloud.enable = true;
    };
  };
  programs.msmtp.enable = false;
  services.znapzend.zetup."zdata/big-apps/nextcloud".destinations."marabethia.vanpetegem.me" = {
    plan = "1day=>1hour,1week=>1day,4week=>1week,1year=>1month,10year=>6month";
    host = "marabethia.vanpetegem.me";
    dataset = "zroot/safe/services/nextcloud";
  };
}
