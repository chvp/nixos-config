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
      instances.default = {
        enable = true;
        url = "https://git.chvp.be";
        labels = [ ];
        name = config.networking.hostName;
        tokenFile = config.age.secrets."passwords/services/git/token-file".path;
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
    };

    age.secrets."passwords/services/git/token-file" = {
      file = ../../../../secrets/passwords/services/git/token-file.age;
    };
  };
}
