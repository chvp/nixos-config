{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".local/share/direnv"; type = "cache"; }
    { path = ".cache/direnv"; type = "cache"; }
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
      stdlib = ''
        : ''${XDG_CACHE_HOME:=$HOME/.cache}
        hash=$(echo -n $PWD | shasum | cut -d' ' -f 1)
        direnv_layout_dir=$XDG_CACHE_HOME/direnv/layouts/"''$(echo -n $PWD | sed 's#/#-#g')/$hash"
        mkdir -p "$direnv_layout_dir"
      '';
    };
  };
}
