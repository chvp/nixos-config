{ config, lib, pkgs, ... }:

{
  options.chvp.services.torrents = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf config.chvp.services.torrents.enable {
    chvp.services.nginx.hosts = [{ fqdn = "transmission.vanpetegem.me"; basicProxy = "http://localhost:9091"; }];

    services.transmission = {
      enable = true;
      package = pkgs.transmission_4.overrideAttrs (old: rec {
        version = "4.1.2";
        src = pkgs.fetchFromGitHub {
          owner = "transmission";
          repo = "transmission";
          tag = version;
          hash = "sha256-FI/qH0VqhEjiN+31UCOiDLWkyucMKfH4i0bYW7lceQk=";
          fetchSubmodules = true;
        };
      });
      user = "charlotte";
      group = "users";
      home = "/var/lib/transmission";
      openRPCPort = false;
      openPeerPorts = true;
      credentialsFile = config.age.secrets."files/programs/transmission/config.json".path;
      settings = {
        umask = 18;
        download-dir = "/srv/data";
        incomplete-dir = "/srv/data/.incomplete";
        rpc-authentication-required = true;
        rpc-bind-address = "0.0.0.0";
        rpc-enabled = true;
        rpc-host-whitelist-enabled = false;
        rpc-whitelist-enabled = false;
        speed-limit-down = 51200;
        speed-limit-down-enabled = true;
      };
    };

    systemd.services.transmission.serviceConfig.TimeoutStartSec = 60 * 10;
    age.secrets."files/programs/transmission/config.json" = {
      file = ../../../../secrets/files/programs/transmission/config.json.age;
      owner = "charlotte";
    };
  };
}
