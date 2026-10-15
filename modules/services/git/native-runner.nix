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
      instances = lib.genAttrs (lib.genList (i: "chvp-${i}") 2) (name: {
        enable = true;
        inherit name;
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
        tokenFile = config.age.secrets."passwords/services/git/personal-token-file".path;
        settings = {
          container.enable_ipv6 = true;
        };
      });
    };

    age.secrets."passwords/services/git/personal-token-file" = {
      file = ../../../secrets/passwords/services/git/personal-token-file.age;
    };
  };
}
