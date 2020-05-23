{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".local/share/direnv"; type = "cache"; }
    { path = ".cache/lorri"; type = "cache"; }
  ];

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
  };
}
