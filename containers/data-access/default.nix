{ config, ... }:

{
  imports = [ ./secret.nix ];

  config = {
    chvp.hasContainers = true;

    containers.data-access = {
      ephemeral = true;
      autoStart = true;
      bindMounts = {
        "/home/data/data" = {
          hostPath = "/srv/data";
          isReadOnly = false;
        };
        "/var/secrets" = {
          hostPath = "${config.chvp.dataPrefix}/var/secrets/data-access";
          isReadOnly = true;
        };
      };
      privateNetwork = true;
      hostAddress = "192.168.100.10";
      hostAddress6 = "fc00::1";
      localAddress = "192.168.100.11";
      localAddress6 = "fc00::2";
      config = import ./config.nix;
    };
  };
}
