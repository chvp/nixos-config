{
  config,
  lib,
  pkgs,
  ...
}:

let
  baseDirenv = {
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
      config.global.load_dotenv = true;
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
    enableGc = lib.mkOption {
      default = true;
      example = false;
    };
    slowGc = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = {
    nix = {
      gc = lib.mkIf config.chvp.base.nix.enableGc {
        automatic = true;
        dates = if config.chvp.base.nix.slowGc then "weekly" else "hourly";
        options = "--delete-older-than 7d";
      };
      optimise = {
        automatic = true;
        dates = [ "hourly" ];
      };
      settings = {
        substituters = [
          "https://cache.nixos.org"
          "https://accentor.cachix.org"
          "https://attic.chvp.be/chvp"
          "https://nix-community.cachix.org"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "accentor.cachix.org-1:QP+oJwzmeq5Fsyp4Vk501UgUSbl5VIna/ard/XOePH8="
          "chvp:rr0HX8qgfg1VKDTMgDPuFgOz8qTf/Le3stet9AMY0NM="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
        trusted-users = [ username ];
      };
      extraOptions = lib.mkIf config.chvp.base.nix.enableDirenv ''
        keep-outputs = true
        keep-derivations = true
      '';
    };
    programs.command-not-found.enable = false;

    chvp.base = {
      emacs = {
        config = {
          nix-mode = lib.hm.dag.entryAnywhere {
            packages = epkgs: [ epkgs.nix-mode ];
            elisp = ''
              ;; Nix syntax support
              (require 'nix-mode)
              (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
            '';
          };
        };
        lateConfig = lib.mkIf config.chvp.base.nix.enableDirenv {
          envrc = lib.hm.dag.entryAfter [ "general" ] {
            packages = epkgs: [ epkgs.envrc ];
            elisp = ''
              (setopt envrc-async 3)
              (require 'envrc)
              (envrc-global-mode)
              (diminish 'envrc-mode)
              (lmap
                "e"  '(:ignore t :which-key "envrc")
                "ea" '("Allow .envrc" . envrc-allow)
                "ed" '("Deny .envrc" . envrc-deny)
                "er" '("Reload env" . envrc-reload)
              )
            '';
          };
        };
      };
      zfs.homeLinks = [
        {
          path = ".config/cachix";
          type = "cache";
        }
        {
          path = ".config/attic";
          type = "cache";
        }
      ]
      ++ (lib.optional config.chvp.base.nix.enableDirenv {
        path = ".local/share/direnv";
        type = "cache";
      });
    };

    home-manager.users.${username} = lib.recursiveUpdate {
      home.packages = [
        pkgs.cachix
        pkgs.attic-client
      ];
      programs = {
        command-not-found.enable = false;
        nix-index = {
          enable = true;
          package = config.programs.nix-index.package;
        };
      };
    } (lib.optionalAttrs config.chvp.base.nix.enableDirenv baseDirenv);

  };
}
