{ config, lib, pkgs, ... }:

{
  options.chvp.git.email = lib.mkOption {
    type = lib.types.str;
    default = "charlotte@vanpetegem.me";
    example = "charlotte@vanpetegem.me";
    description = ''
      Default email set in global git config.
    '';
  };

  config.home-manager.users.charlotte = { pkgs, ... }: {
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
        signByDefault = true;
      };
      userEmail = config.chvp.git.email;
      userName = "Charlotte Van Petegem";
    };
  };
}
