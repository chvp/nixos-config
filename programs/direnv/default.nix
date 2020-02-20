{ ... }:

{
  home-manager.users.charlotte = { ... }: {
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      config = {
        global = {
          strict_env = true;
        };
      };
    };
    services.lorri.enable = true;
    xdg.dataFile.nix-shells.source = ./shells;
  };
}
