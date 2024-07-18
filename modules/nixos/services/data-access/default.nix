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
        "/home/readonly/data" = {
          hostPath = "/srv/data";
          isReadOnly = true;
        };
        "/run/secrets" = {
          hostPath = "/run/data-access";
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
        system.stateVersion = config.chvp.stateVersion;
        imports = [ ./config.nix ];
      };
    };

    age.secrets."data-access/ssh_host_rsa_key" = {
      file = ../../../../secrets/data-access/ssh_host_rsa_key.age;
      path = "/run/data-access/ssh_host_rsa_key";
      symlink = false;
    };
    age.secrets."data-access/ssh_host_rsa_key.pub" = {
      file = ../../../../secrets/data-access/ssh_host_rsa_key.pub.age;
      path = "/run/data-access/ssh_host_rsa_key.pub";
      symlink = false;
    };
    age.secrets."data-access/ssh_host_ed25519_key" = {
      file = ../../../../secrets/data-access/ssh_host_ed25519_key.age;
      path = "/run/data-access/ssh_host_ed25519_key";
      symlink = false;
    };
    age.secrets."data-access/ssh_host_ed25519_key.pub" = {
      file = ../../../../secrets/data-access/ssh_host_ed25519_key.pub.age;
      path = "/run/data-access/ssh_host_ed25519_key.pub";
      symlink = false;
    };
    age.secrets."data-access/password_file" = {
      file = ../../../../secrets/data-access/password_file.age;
      path = "/run/data-access/password_file";
      symlink = false;
    };
    age.secrets."data-access/readonly_password_file" = {
      file = ../../../../secrets/data-access/readonly_password_file.age;
      path = "/run/data-access/readonly_password_file";
      symlink = false;
    };
    age.secrets."data-access/authorized_keys" = {
      file = ../../../../secrets/data-access/authorized_keys.age;
      owner = "charlotte";
      path = "/run/data-access/data_authorized_keys";
      symlink = false;
    };
    age.secrets."data-access/readonly_authorized_keys" = {
      file = ../../../../secrets/data-access/readonly_authorized_keys.age;
      owner = "1001";
      group = "65534";
      path = "/run/data-access/readonly_authorized_keys";
      symlink = false;
    };
    age.secrets."data-access/create_torrent" = {
      file = ../../../../secrets/data-access/create_torrent.age;
      owner = "charlotte";
      path = "/run/data-access/create_torrent";
      symlink = false;
    };
    age.secrets."passwords/services/data-basic-auth" = {
      file = ../../../../secrets/passwords/services/data-basic-auth.age;
      owner = "nginx";
    };
  };
}
