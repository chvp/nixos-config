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
    home.packages = with pkgs; [ nix-index ];
    programs.zsh.initExtra = ''
      source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
    '';
    systemd.user = {
      services.nix-index = {
        Unit = {
          Description = "Service to run nix-index";
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.nix-index}/bin/nix-index";
        };
      };
      timers.nix-index = {
        Unit = {
          Description = "Timer that starts nix-index every two hours";
          PartOf = [ "nix-index.service" ];
        };
        Timer = {
          OnCalendar = "00/2:30";
        };
        Install = {
          WantedBy = [ "default.target" ];
        };
      };
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
    # Note that this is only enabled for charlotte, until https://github.com/bennofs/nix-index/issues/143 is resolved.
    enableNixIndex = lib.mkOption {
      default = true;
      example = false;
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
      zfs = {
        homeLinks =
          (lib.optional config.chvp.base.nix.enableDirenv { path = ".local/share/direnv"; type = "cache"; }) ++
          (lib.optional config.chvp.base.nix.enableNixIndex { path = ".cache/nix-index"; type = "cache"; });
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
      optimise = {
        automatic = true;
        dates = [ "hourly" ];
      };
      trustedUsers = [ "@wheel" ];
      extraOptions = ''
        substituters = https://cache.nixos.org https://nix-community.cachix.org
        trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
      '' + (lib.optionalString config.chvp.base.nix.enableDirenv ''
        keep-outputs = true
        keep-derivations = true
      '');
    };

    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) config.chvp.base.nix.unfreePackages;
    nixpkgs.overlays = [
      (self: super: {
        nix = super.nix_2_4;
      })
    ];

    home-manager.users.charlotte = { ... }:
      lib.recursiveUpdate
        (lib.optionalAttrs config.chvp.base.nix.enableDirenv baseDirenv)
        (lib.optionalAttrs config.chvp.base.nix.enableNixIndex baseNixIndex);
    home-manager.users.root = { ... }: lib.optionalAttrs config.chvp.base.nix.enableDirenv baseDirenv;
  };
}
