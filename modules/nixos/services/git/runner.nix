{ config, lib, pkgs, ... }:

{
  options.chvp.services.git.runner.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.git.runner.enable {
    networking.firewall.trustedInterfaces = [ "br-+" ];
    services.gitea-actions-runner = {
      package = pkgs.forgejo-actions-runner;
      instances.default = {
        enable = true;
        url = "https://git.chvp.be";
        labels = [ ];
        name = config.networking.hostName;
        tokenFile = config.age.secrets."passwords/services/git/token-file".path;
        settings.container.enable_ipv6 = false;
      };
    };
    virtualisation.docker = {
      enable = true;
      daemon.settings.ipv6 = false;
    };

    age.secrets."passwords/services/git/token-file" = {
      file = ../../../../secrets/passwords/services/git/token-file.age;
    };
  };
}
