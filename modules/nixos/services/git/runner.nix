{ config, lib, pkgs, ... }:

{
  options.chvp.services.git.runner.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.git.runner.enable {
    networking.firewall.trustedInterfaces = [ "br-+" ];
    services.gitea-actions-runner = {
      package = pkgs.forgejo-runner;
      instances.global-1 = {
        enable = true;
        url = "https://git.chvp.be";
        labels = [ "docker:docker://node:lts" ];
        name = "global-1";
        tokenFile = config.age.secrets."passwords/services/git/token-file".path;
        settings = {
          container.enable_ipv6 = true;
        };
      };
      instances.global-2 = {
        enable = true;
        url = "https://git.chvp.be";
        labels = [ "docker:docker://node:lts" ];
        name = "global-2";
        tokenFile = config.age.secrets."passwords/services/git/token-file".path;
        settings = {
          container.enable_ipv6 = true;
        };
      };
      instances.chvp-1 = {
        enable = true;
        url = "https://git.chvp.be";
        labels = [ "native:host" ];
        name = "chvp-1";
        tokenFile = config.age.secrets."passwords/services/git/personal-token-file".path;
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
      autoPrune.enable = true;
    };

    age.secrets."passwords/services/git/token-file" = {
      file = ../../../../secrets/passwords/services/git/token-file.age;
    };
    age.secrets."passwords/services/git/personal-token-file" = {
      file = ../../../../secrets/passwords/services/git/personal-token-file.age;
    };
  };
}
