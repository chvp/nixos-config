{
  config,
  lib,
  pkgs,
  ...
}:

let
  username = config.chvp.username;
in
{
  options.chvp.services.torrents = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf config.chvp.services.torrents.enable {
    chvp.services.nginx.hosts = [
      {
        fqdn = "transmission.vanpetegem.me";
        options = {
          basicAuthFile = config.age.secrets."passwords/services/torrents-basic-auth".path;
          locations = {
            "/" = {
              proxyPass = "http://localhost:9091";
              extraConfig = ''
                proxy_set_header X-Forwarded-Ssl on;
              '';
            };
            "/sonarr" = {
              proxyPass = "http://localhost:8989";
              extraConfig = ''
                proxy_set_header X-Forwarded-Ssl on;
              '';
            };
            "/radarr" = {
              proxyPass = "http://localhost:7878";
              extraConfig = ''
                proxy_set_header X-Forwarded-Ssl on;
              '';
            };
          };
        };
      }
    ];

    services = {
      transmission = {
        enable = true;
        package = pkgs.transmission_4.overrideAttrs (old: {
          version = "4.1.4";
          src = old.src.overrideAttrs (oldSrc: {
            hash = "sha256-gkb0Hl+ZVBhQCQSMapllRhoz4uK/lSv6o+Abpvxpyrs=";
          });
        });
        user = username;
        group = "users";
        home = "/var/lib/transmission";
        openRPCPort = false;
        openPeerPorts = true;
        settings = {
          umask = 18;
          download-dir = "/srv/data";
          incomplete-dir = "/srv/data/.incomplete";
          rpc-authentication-required = false;
          rpc-bind-address = "0.0.0.0";
          rpc-enabled = true;
          rpc-host-whitelist-enabled = false;
          rpc-whitelist-enabled = false;
          speed-limit-down = 51200;
          speed-limit-down-enabled = true;
        };
      };
      radarr = {
        enable = true;
        user = username;
        group = "users";
        dataDir = "/var/lib/transmission/.config/radarr";
        settings = {
          auth.authentication_method = "External";
          server.urlbase = "/radarr";
        };
      };
      sonarr = {
        enable = true;
        user = username;
        group = "users";
        dataDir = "/var/lib/transmission/.config/sonarr";
        settings = {
          auth.authentication_method = "External";
          server.urlbase = "/sonarr";
        };
      };
    };

    systemd.services.transmission.serviceConfig.TimeoutStartSec = 60 * 10;
    age.secrets."passwords/services/torrents-basic-auth" = {
      file = ../../../secrets/passwords/services/torrents-basic-auth.age;
      owner = "nginx";
    };
  };
}
