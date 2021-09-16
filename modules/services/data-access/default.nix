{ config, lib, ... }:

{
  options.chvp.services.data-access.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.data-access.enable {
    chvp.services = {
      containers.enable = true;
      nginx.hosts = [
        {
          fqdn = "data.vanpetegem.me";
          options = {
            default = true;
            basicAuthFile = config.age.secrets."passwords/services/data-basic-auth".path;
            root = "/srv/data";
            locations = {
              "/".extraConfig = ''
                autoindex on;
              '';
              "/public".extraConfig = ''
                autoindex on;
                auth_basic off;
              '';
            };
          };
        }
      ];
    };

    networking.firewall.allowedTCPPorts = [ 2002 ];

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
      forwardPorts = [{
        containerPort = 22;
        hostPort = 2002;
        protocol = "tcp";
      }];
      privateNetwork = true;
      hostAddress = "192.168.100.10";
      hostAddress6 = "fc00::1";
      localAddress = "192.168.100.11";
      localAddress6 = "fc00::2";
      config = { ... }: {
        imports = [ ./config.nix ];
      };
    };

    age.secrets."data-access/ssh_host_rsa_key".file = ../../../secrets/data-access/ssh_host_rsa_key.age;
    age.secrets."data-access/ssh_host_rsa_key.pub".file = ../../../secrets/data-access/ssh_host_rsa_key.pub.age;
    age.secrets."data-access/ssh_host_ed25519_key".file = ../../../secrets/data-access/ssh_host_ed25519_key.age;
    age.secrets."data-access/ssh_host_ed25519_key.pub".file = ../../../secrets/data-access/ssh_host_ed25519_key.pub.age;
    age.secrets."data-access/password_file".file = ../../../secrets/data-access/password_file.age;
    age.secrets."data-access/authorized_keys" = {
      file = ../../../secrets/data-access/authorized_keys.age;
      owner = "charlotte";
    };
    age.secrets."data-access/create_torrent" = {
      file = ../../../secrets/data-access/create_torrent.age;
      owner = "charlotte";
    };
    age.secrets."passwords/services/data-basic-auth" = {
      file = ../../../secrets/passwords/services/data-basic-auth.age;
      owner = "nginx";
    };
  };
}
