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
      user = "charlotte";
      group = "users";
      home = "/data/var/lib/transmission";
      openRPCPort = false;
      openPeerPorts = true;
      credentialsFile = config.age.secrets."files/programs/transmission/config.json".path;
      settings = {
        umask = 18;
        download-dir = "/srv/data";
        rpc-authentication-required = true;
        rpc-bind-address = "0.0.0.0";
        rpc-enabled = true;
        rpc-host-whitelist-enabled = false;
        rpc-whitelist-enabled = false;
      };
    };

    age.secrets."files/programs/transmission/config.json" = {
      file = ../../../secrets/files/programs/transmission/config.json.age;
      owner = "charlotte";
    };
  };
}
