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
        "/run/secrets" = {
          hostPath = "/run/secrets/data-access";
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

    age.secrets."data-access/ssh_host_rsa_key".file = ../../secrets/data-access/ssh_host_rsa_key.age;
    age.secrets."data-access/ssh_host_rsa_key.pub".file = ../../secrets/data-access/ssh_host_rsa_key.pub.age;
    age.secrets."data-access/ssh_host_ed25519_key".file = ../../secrets/data-access/ssh_host_ed25519_key.age;
    age.secrets."data-access/ssh_host_ed25519_key.pub".file = ../../secrets/data-access/ssh_host_ed25519_key.pub.age;
  };
}
