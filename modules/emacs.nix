{ config, lib, pkgs, ... }:

let
  emacsPkg = pkgs.emacsWithPackagesFromUsePackage {
    config = ./emacs/init.el;
    package = pkgs.emacsPgtk;
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
