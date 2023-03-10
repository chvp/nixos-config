{ config, lib, pkgs, ... }:
let
  baseDirenv = {
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
  };
  baseNixIndex = {
    programs.command-not-found.enable = false;
    programs.nix-index = {
      enable = true;
      package = config.programs.nix-index.package;
    };
  };
in
{
  options.chvp.base.nix = {
    enableDirenv = lib.mkOption {
      default = true;
      example = false;
    };
    unfreePackages = lib.mkOption {
      default = [ ];
      example = [ "teams" ];
    };
  };

  config = {
    programs.command-not-found.enable = false;
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
      zfs = {
        homeLinks =
          (lib.optional config.chvp.base.nix.enableDirenv { path = ".local/share/direnv"; type = "cache"; });
        systemLinks =
          (lib.optional config.chvp.base.nix.enableDirenv { path = "/root/.local/share/direnv"; type = "cache"; });
      };
    };

    nix = {
      gc = {
        automatic = true;
        dates = "hourly";
        options = "--delete-older-than 7d";
      };
      package = pkgs.nixStable;
      settings = {
        auto-optimise-store = true;
        substituters = [
          "https://cache.nixos.org"
          "https://nix-community.cachix.org"
          "https://chvp.cachix.org"
          "https://accentor.cachix.org"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "chvp.cachix.org-1:eIG26KkeA+R3tCpvmaayA9i3KVVL06G+qB5ci4dHBT4="
          "accentor.cachix.org-1:QP+oJwzmeq5Fsyp4Vk501UgUSbl5VIna/ard/XOePH8="
        ];
        trusted-users = [ "@wheel" ];
      };
      extraOptions = lib.mkIf config.chvp.base.nix.enableDirenv ''
        keep-outputs = true
        keep-derivations = true
      '';
    };

    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) config.chvp.base.nix.unfreePackages;

    home-manager.users.charlotte = { ... }:
      lib.recursiveUpdate
        (lib.optionalAttrs config.chvp.base.nix.enableDirenv baseDirenv)
        baseNixIndex;
    home-manager.users.root = { ... }: baseNixIndex;
  };
}
