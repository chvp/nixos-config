{ config, lib, pkgs, ... }:

{
  options.chvp.emacs = {
    enable = lib.mkOption {
      default = true;
      example = false;
    };
  };

  config = lib.mkIf config.chvp.emacs.enable {
    home-manager.users.charlotte = { ... }: {
      programs.emacs = {
        enable = true;
        package = pkgs.emacsWithPackagesFromUsePackage {
          config = ./emacs/init.el;
          package = pkgs.emacsPgtk;
          alwaysEnsure = true;
          extraEmacsPackages = epkgs: [
            # mu4e is included in the mu package and should be used from there
            pkgs.mu
          ];
        };
      };
      home.file = {
        ".emacs.d/early-init.el".source = ./emacs/early-init.el;
        ".emacs.d/init.el".source = ./emacs/init.el;
      };
    };
  };
}
