{ config, lib, pkgs, ... }:

{
  options.chvp.base.emacs = {
    fullConfig = lib.mkOption {
      readOnly = true;
      default = builtins.readFile ./base-init.el + (lib.concatStringsSep "\n" config.chvp.base.emacs.extraConfig) + ''
        (provide 'init)
        ;;; init.el ends here
      '';
    };
    extraConfig = lib.mkOption {
      default = [ ];
    };
    package = lib.mkOption {
      readOnly = true;
      default = pkgs.emacsWithPackagesFromUsePackage {
        config = config.chvp.base.emacs.fullConfig;
        package = pkgs.emacs-pgtk;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: lib.optional config.chvp.graphical.mail.enable epkgs.mu4e;
      };
    };
  };

  config = {
    chvp.base.zfs.homeLinks = [
      { path = ".cache/emacs"; type = "cache"; }
    ];

    home-manager.users.charlotte = { ... }: {
      services.emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
        package = config.chvp.base.emacs.package;
      };
      home = {
        packages = [
          (pkgs.writeShellScriptBin "emacs" ''${config.chvp.base.emacs.package}/bin/emacsclient -c "$@"'')
          (pkgs.writeShellScriptBin "emacsclient" ''${config.chvp.base.emacs.package}/bin/emacsclient "$@"'')
        ];
        sessionVariables = { EDITOR = "emacs"; };
      };
      xdg.configFile = {
        "emacs/init.el".text = config.chvp.base.emacs.fullConfig;
        "emacs/early-init.el".source = ./early-init.el;
      };
    };
  };
}
