{ config, lib, ... }:

{
  options.chvp.syncthing-server.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.syncthing-server.enable {
    services.syncthing = {
      enable = true;
      dataDir = "${config.chvp.dataPrefix}/var/lib/syncthing";
      configDir = "${config.chvp.dataPrefix}/var/lib/syncthing/.config";
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8384";
    };

    chvp.nginx.hosts = [
      {
        fqdn = "syncthing.vanpetegem.me";
        basicProxy = "http://localhost:8384";
        options = {
          basicAuthFile = config.age.secrets."passwords/services/syncthing-basic-auth".path;
        };
      }
    ];
    age.secrets."passwords/services/syncthing-basic-auth" = {
      file = ../secrets/passwords/services/syncthing-basic-auth.age;
      owner = "nginx";
    };
  };
}
