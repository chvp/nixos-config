{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".local/share/direnv"; type = "cache"; }
  ];

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  home-manager.users.charlotte = { ... }: {
    programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
      enableZshIntegration = true;
      config = {
        global = {
          strict_env = true;
        };
      };
    };
  };
}
