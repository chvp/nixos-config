{
  options,
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

  config = lib.mkIf config.chvp.services.git.native-runner.enable (
    if builtins.hasAttr "forgejo-runner" options.services then
      {
        services.forgejo-runner.instances."chvp-personal-0" = {
          enable = true;
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
          secrets.server.connections."git.chvp.be".token_url =
            config.age.secrets."passwords/services/git/chvp-personal-0-token-file".path;
          settings = {
            runner = {
              capacity = 2;
              labels = [ "native:host" ];
            };
            server.connections."git.chvp.be" = {
              url = "https://git.chvp.be";
              uuid = "1b5b0fd5-0515-4eb9-90ad-eb55f00df673";
            };
          };
        };

        age.secrets."passwords/services/git/chvp-personal-0-token-file".file =
          ../../../secrets/passwords/services/git/chvp-personal-0-token-file.age;
      }
    else
      { }
  );
}
