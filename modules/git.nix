{ config, lib, pkgs, ... }:

{
  options.chvp.git = {
    enable = lib.mkOption {
      default = true;
      example = false;
    };
    email = lib.mkOption {
      type = lib.types.str;
      default = "charlotte@vanpetegem.me";
      example = "charlotte@vanpetegem.me";
      description = ''
        Default email set in global git config.
      '';
    };
  };

  config =
    let
      base = {
        home.packages = with pkgs; [
          gitAndTools.gitflow
          git-crypt
        ];
        programs.git = {
          enable = true;
          extraConfig = {
            branch = {
              autosetuprebase = "always";
            };
            pull = {
              rebase = true;
            };
          };
          ignores = [
            ".direnv"
            ".envrc"
            "shell.nix"
            # Ruby dependencies in source tree
            "/vendor/bundle"
            "**/*.patch"
          ];
          signing = {
            key = "charlotte@vanpetegem.me";
            signByDefault = config.chvp.graphical;
          };
          userEmail = config.chvp.git.email;
          userName = "Charlotte Van Petegem";
        };
      };
    in
    lib.mkIf config.chvp.git.enable {
      home-manager.users.charlotte = { ... }: base;
      home-manager.users.root = { ... }: base;
    };
}
