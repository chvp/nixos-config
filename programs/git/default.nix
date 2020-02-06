{ config, lib, pkgs, ... }:

{
  options.custom.git.email = lib.mkOption {
    type = lib.types.str;
    default = "charlotte@vanpetegem.me";
    example = "charlotte@vanpetegem.me";
    description = ''
      Default email set in git config
    '';
  };

  config.home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.git-crypt ];
    programs.git = {
      enable = true;
      extraConfig = {
        branch = {
          setupautorebase = "always";
        };
      };
      ignores = [
        "**/*.patch"
      ];
      userEmail = config.custom.git.email;
      userName = "Charlotte Van Petegem";
    };
  };
}
