{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.chvp.services.git.native-runner.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.git.native-runner.enable {
    services.gitea-actions-runner = {
      package = pkgs.forgejo-runner;
      instances.chvp-1 = {
        enable = true;
        url = "https://git.chvp.be";
        hostPackages = with pkgs; [
          attic-client
          bash
          coreutils
          curl
          gawk
          gitMinimal
          gnused
          nix
          nodejs
          wget
        ];
        labels = [ "native:host" ];
        name = "chvp-1";
        tokenFile = config.age.secrets."passwords/services/git/personal-token-file".path;
        settings = {
          container.enable_ipv6 = true;
        };
      };
    };

    age.secrets."passwords/services/git/personal-token-file" = {
      file = ../../../secrets/passwords/services/git/personal-token-file.age;
    };
  };
}
