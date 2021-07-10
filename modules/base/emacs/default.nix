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
        package = pkgs.emacsPgtk;
        alwaysEnsure = true;
        # mu4e is included in the mu package and should be used from there
        extraEmacsPackages = epkgs: lib.optional config.chvp.graphical.mail.enable pkgs.mu;
      };
    };
  };

  config = {
    nixpkgs.overlays = [
      (self: super: {
        mu = super.mu.overrideAttrs (old: {
          version = "1.5.13";
          src = self.fetchFromGitHub {
            owner = "djcb";
            repo = "mu";
            rev = "6d67e146fecb5aa512a7eff4b8044225af0dc5ce";
            sha256 = "0ip7nd7z2l60a3dc1aic34hpab4alb0rmxlk9778nz3v88735iik";
          };
        });
      })
    ];
    chvp.base.zfs.homeLinks = [
      { path = ".emacs.d"; type = "cache"; }
    ];

    home-manager.users.charlotte = { ... }: {
      services.emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
        package = config.chvp.base.emacs.package;
      };
      home = {
        file = {
          ".emacs.d/early-init.el".source = ./early-init.el;
          ".emacs.d/init.el".text = config.chvp.base.emacs.fullConfig;
        };
        packages = [
          (pkgs.writeShellScriptBin "emacs" ''${config.chvp.base.emacs.package}/bin/emacsclient -c "$@"'')
          (pkgs.writeShellScriptBin "emacsclient" ''${config.chvp.base.emacs.package}/bin/emacsclient "$@"'')
        ];
        sessionVariables = { EDITOR = "emacs"; };
      };
    };
  };
}
