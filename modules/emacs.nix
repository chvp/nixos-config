{ config, lib, pkgs, ... }:

let
  emacsConfigText = builtins.readFile ./emacs/init.el + (if config.chvp.mail-client.enable then config.chvp.mail-client.mu4eConfig else "") + ''
    (provide 'init)
    ;;; init.el ends here
  '';
  emacsPkg = pkgs.emacsWithPackagesFromUsePackage {
    config = emacsConfigText;
    package = pkgs.emacsPgtk;
    alwaysEnsure = true;
    # mu4e is included in the mu package and should be used from there
    extraEmacsPackages = epkgs: lib.optional config.chvp.mail-client.enable pkgs.mu;
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
          ".emacs.d/init.el".text = emacsConfigText;
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
