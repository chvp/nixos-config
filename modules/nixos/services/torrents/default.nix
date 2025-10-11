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

    nixpkgs.overlays = [
      (self: super: {
        transmission_4 = super.transmission_4.overrideAttrs (old: {
          version = "4.0.5";
          src = pkgs.fetchFromGitHub {
            owner = "transmission";
            repo = "transmission";
            rev = "4.0.5";
            hash = "sha256-gd1LGAhMuSyC/19wxkoE2mqVozjGPfupIPGojKY0Hn4=";
            fetchSubmodules = true;
          };
        });
      })
    ];

    services.transmission = {
      enable = true;
      package = pkgs.transmission_4;
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
