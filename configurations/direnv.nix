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
          local profile_dir="$(direnv_layout_dir)/flake-profile"
          eval "$(nix print-dev-env --profile "''${profile_dir}")"
          local stripped_pwd=''${PWD/\//}
          local escaped_pwd=''${stripped_pwd//-/--}
          local escaped_pwd=''${escaped_pwd//\//-}
          ln -fs "''${profile_dir}" "/nix/var/nix/gcroots/per-user/$USER/''${escaped_pwd}"
        }
      '';
    };
  };
}
