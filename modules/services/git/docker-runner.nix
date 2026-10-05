{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.chvp.services.git.docker-runner.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.git.docker-runner.enable {
    microvm.vms = {
      actions-runner = {
        config = {
          microvm = {
            mem = 8192;
            vcpu = 4;
            interfaces = [
              {
                type = "user";
                id = "ve-vm-a1";
                mac = "02:00:00:00:00:01";
              }
            ];
            shares = [
              {
                source = "/nix/store";
                mountPoint = "/nix/.ro-store";
                readOnly = true;
                tag = "ro-store";
              }
              {
                source = "/run/actions-runner";
                mountPoint = "/run/secrets";
                readOnly = true;
                tag = "secrets";
              }
            ];
            volumes = [
              {
                autoCreate = true;
                image = "${config.chvp.dataPrefix}/var/lib/actions-runner/containers.image";
                label = "containers";
                mountPoint = "/var/lib/docker";
                size = 20 * 1024;
              }
            ];
          };
          system.stateVersion = config.chvp.stateVersion;
          services.openssh.settings.PermitRootLogin = "yes";
          services.gitea-actions-runner = {
            package = pkgs.forgejo-runner;
            instances.global-1 = {
              enable = true;
              url = "https://git.chvp.be";
              labels = [ "docker:docker://node:lts" ];
              name = "global-1";
              tokenFile = "/run/secrets/token-file";
              settings = {
                container.enable_ipv6 = true;
              };
            };
            instances.global-2 = {
              enable = true;
              url = "https://git.chvp.be";
              labels = [ "docker:docker://node:lts" ];
              name = "global-2";
              tokenFile = "/run/secrets/token-file";
              settings = {
                container.enable_ipv6 = true;
              };
            };
          };
          virtualisation.docker = {
            enable = true;
            daemon.settings = {
              fixed-cidr-v6 = "fd00::/80";
              ipv6 = true;
            };
            autoPrune = {
              enable = true;
              dates = "hourly";
            };
          };
        };
      };
    };

    age.secrets."passwords/services/git/token-file" = {
      file = ../../../secrets/passwords/services/git/token-file.age;
      path = "/run/actions-runner/token-file";
      owner = "microvm";
      symlink = false;
    };
  };
}
