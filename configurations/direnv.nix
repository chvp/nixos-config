{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".local/share/direnv"; type = "cache"; }
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
      stdlib = ''
        use_flake() {
          watch_file flake.nix
          watch_file flake.lock
          eval "$(nix --experimental-features 'nix-commnand flakes' print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
        }
      '';
    };
  };
}
