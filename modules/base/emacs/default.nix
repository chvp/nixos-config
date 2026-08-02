{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.base.emacs = {
    extraConfig = lib.mkOption {
      default = [ ];
    };
    extraPackages = lib.mkOption {
      default = [ ];
    };
    fullConfig = lib.mkOption {
      readOnly = true;
      default = builtins.readFile ./base-init.el + (lib.concatStringsSep "\n" config.chvp.base.emacs.extraConfig) + ''
        (provide 'init)
        ;;; init.el ends here
      '';
    };
    package = lib.mkOption {
      readOnly = true;
      default = pkgs.emacsWithPackagesFromUsePackage {
        config = config.chvp.base.emacs.fullConfig;
        package = pkgs.emacs-pgtk;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: builtins.foldl' (xs: ys: xs ++ ys) [ ] (builtins.map (fun: (fun epkgs)) config.chvp.base.emacs.extraPackages);
      };
    };
  };

  config = {
    chvp.base.zfs.homeLinks = [
      { path = ".cache/emacs"; type = "cache"; }
    ];
    home-manager.users.${username} = { ... }: {
      home = {
        file = {
          ".emacs.d/init.el".text = config.chvp.base.emacs.fullConfig;
          ".emacs.d/early-init.el".source = ./early-init.el;
        };
        packages = [
          (pkgs.writeShellScriptBin "emacs" ''${config.chvp.base.emacs.package}/bin/emacsclient -c "$@"'')
          (pkgs.writeShellScriptBin "emacsclient" ''${config.chvp.base.emacs.package}/bin/emacsclient "$@"'')
        ];
        sessionVariables = { EDITOR = "emacs"; };
      };
      services.emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
        package = config.chvp.base.emacs.package;
      };
    };
  };
}
