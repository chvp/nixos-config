{ config, lib, pkgs, ... }:

let
  baseDirenv = {
    home.sessionVariables = {
      DIRENV_LOG_FORMAT = "";
    };
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
      config.global.load_dotenv = true;
    };
  };
  baseNixIndex = {
    programs.command-not-found.enable = false;
    programs.nix-index = {
      enable = true;
      package = config.programs.nix-index.package;
    };
  };
  username = config.chvp.username;
in
{
  options.chvp.base.nix = {
    enableDirenv = lib.mkOption {
      default = true;
      example = false;
    };
    slowGc = lib.mkOption {
      default = false;
      example = true;
    };
    # Used in /flake.nix, since we have to use it at nixpkgs import time
    unfreePackages = lib.mkOption {
      default = [ ];
      example = [ "teams" ];
    };
  };

  config = {
    chvp.base = {
      emacs.extraConfig = [
        ''
          ;; Nix syntax support
          (use-package nix-mode
            :mode "\\.nix\\'"
            )
        ''
      ] ++ lib.optional config.chvp.base.nix.enableDirenv ''
        ;; Direnv integration in emacs.
        (use-package direnv
          :config (direnv-mode)
          )
      '';
      zfs.homeLinks = (lib.optional config.chvp.base.nix.enableDirenv { path = ".local/share/direnv"; type = "cache"; });
    };
    nix = {
      gc = {
        automatic = true;
        options = "--delete-older-than 7d";
      };
      optimise.automatic = true;
      settings = {
        substituters = [
          "https://cache.nixos.org"
          "https://accentor.cachix.org"
          "https://chvp.cachix.org"
          "https://lanzaboote.cachix.org"
          "https://nix-community.cachix.org"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "accentor.cachix.org-1:QP+oJwzmeq5Fsyp4Vk501UgUSbl5VIna/ard/XOePH8="
          "chvp.cachix.org-1:eIG26KkeA+R3tCpvmaayA9i3KVVL06G+qB5ci4dHBT4="
          "lanzaboote.cachix.org-1:Nt9//zGmqkg1k5iu+B3bkj3OmHKjSw9pvf3faffLLNk="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
        trusted-users = [ username ];
      };
      extraOptions = lib.mkIf config.chvp.base.nix.enableDirenv ''
        keep-outputs = true
        keep-derivations = true
      '';
    };

    home-manager.users.${username} = { ... }:
      lib.recursiveUpdate
        (lib.optionalAttrs config.chvp.base.nix.enableDirenv baseDirenv)
        baseNixIndex;
  };
}
