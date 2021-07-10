{ config, lib, ... }:

{
  options.chvp.services.syncthing.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.syncthing.enable {
    services.syncthing = {
      enable = true;
      dataDir = "${config.chvp.dataPrefix}/var/lib/syncthing";
      configDir = "${config.chvp.dataPrefix}/var/lib/syncthing/.config";
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8384";
    };

    chvp.services.nginx.hosts = [
      {
        fqdn = "syncthing.vanpetegem.me";
        basicProxy = "http://localhost:8384";
        options.basicAuthFile = config.age.secrets."passwords/services/syncthing-basic-auth".path;
      }
    ];

    age.secrets."passwords/services/syncthing-basic-auth" = {
      file = ../../../secrets/passwords/services/syncthing-basic-auth.age;
      owner = "nginx";
    };
  };
}
