{ config, lib, pkgs, ... }:

let
  emacsPkg = pkgs.emacsWithPackagesFromUsePackage {
    config = ./emacs/init.el;
    package = pkgs.emacsPgtkGcc;
    alwaysEnsure = true;
    extraEmacsPackages = epkgs: [
      # mu4e is included in the mu package and should be used from there
      pkgs.mu
    ];
  };
in
{
  options.chvp.emacs = {
    enable = lib.mkOption {
      default = true;
      example = false;
    };
    package = lib.mkOption { };
  };

  config = lib.mkIf config.chvp.emacs.enable {
    nixpkgs.overlays = [
      (self: super: {
        mu = super.mu.overrideAttrs (old: {
          version = "1.5.12";
          src = self.fetchFromGitHub {
            owner = "djcb";
            repo = "mu";
            rev = "c5219778af65dc868eea91c910c801e5105d5cd7";
            sha256 = "1z6l02bdsh4z2yv3dbixrs0fvyp2axx62dnmqwyampjn5xs5l2yf";
          };
        });
      })
    ];
    chvp = {
      emacs.package = emacsPkg;
      zfs.homeLinks = [
        { path = ".emacs.d"; type = "cache"; }
      ];
    };

    home-manager.users.charlotte = { ... }: {
      services.emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
        package = emacsPkg;
      };
      home = {
        file = {
          ".emacs.d/early-init.el".source = ./emacs/early-init.el;
          ".emacs.d/init.el".source = ./emacs/init.el;
        };
        packages = [
          (pkgs.writeShellScriptBin "emacs" ''${emacsPkg}/bin/emacsclient -c "$@"'')
          (pkgs.writeShellScriptBin "emacsclient" ''${emacsPkg}/bin/emacsclient "$@"'')
        ];
        sessionVariables = { EDITOR = "emacs"; };
      };
    };
  };
}
