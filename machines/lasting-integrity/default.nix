{ pkgs, ... }:

{
  imports = [ ./hardware.nix ];

  time.timeZone = "Europe/Berlin";

  networking = {
    hostId = "b352adfe";
    firewall.allowedTCPPorts = [ 25 143 465 587 993 4190 ];
  };

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
        backups = [{
          path = "zroot/safe/data";
          remotePath = "zdata/recv/lasting-integrity/safe/data";
          fast = true;
          location = "192.168.0.1";
        }];
        rootDataset = "zroot/local/root";
      };
    };
    development = {
      docker.enable = true;
      git.enable = true;
    };
    games.tetris.server = true;
    services = {
      matrix.enable = true;
      nginx = {
        extraPostACMEScripts = [
          ''
            cp fullchain.pem /data/root/mailcow/data/assets/ssl/cert.pem
            cp key.pem /data/root/mailcow/data/assets/ssl/key.pem
            pushd /data/root/mailcow
            ${pkgs.bash}/bin/bash -c "source mailcow.conf && ${pkgs.docker-compose}/bin/docker-compose restart"
            popd
          ''
        ];
        hosts = [
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
                  "/.well-known/matrix" = matrixRedirect;
                  "/".return = "307 https://www.vanpetegem.me$request_uri";
                };
            };
          }
          { fqdn = "www.vanpetegem.me"; }
          {
            fqdn = "cvpetegem.be";
            options = {
              locations."/".return = "307 https://www.cvpetegem.be$request_uri";
            };
          }
          { fqdn = "www.cvpetegem.be"; }
          {
            fqdn = "chvp.be";
            options = {
              locations."/".return = "307 https://www.chvp.be$request_uri";
            };
          }
          { fqdn = "www.chvp.be"; }
          {
            fqdn = "mail.vanpetegem.me";
            basicProxy = "http://127.0.0.1:8080";
          }
        ];
      };
      nextcloud.enable = true;
      syncthing.enable = true;
      tunnel.enable = true;
    };
  };
}
