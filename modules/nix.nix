{ config, lib, pkgs, ... }:
let
  baseDirenv = {
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNixDirenvIntegration = true;
    };
  };
  baseUnfree = {
    xdg.configFile."nixpkgs/config.nix".source = ./nix/unfree.nix;
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
  options.chvp.nix = {
    enableDirenv = lib.mkOption {
      default = true;
      example = false;
    };
    enableFlakes = lib.mkOption {
      default = true;
      example = false;
    };
    enableUnfree = lib.mkOption {
      default = false;
      example = true;
    };
    # Note that this is only enabled for charlotte, until https://github.com/bennofs/nix-index/issues/143 is resolved.
    enableNixIndex = lib.mkOption {
      default = true;
      example = false;
    };
  };

  config = {
    chvp.zfs.homeLinks =
      (lib.optional config.chvp.nix.enableDirenv { path = ".local/share/direnv"; type = "cache"; }) ++
      (lib.optional config.chvp.nix.enableNixIndex { path = ".cache/nix-index"; type = "cache"; });
    chvp.zfs.systemLinks =
      (lib.optional config.chvp.nix.enableDirenv { path = "/root/.local/share/direnv"; type = "cache"; });

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
      extraOptions = (lib.optionalString config.chvp.nix.enableDirenv ''
        keep-outputs = true
        keep-derivations = true
      '') + (lib.optionalString config.chvp.nix.enableFlakes ''
        experimental-features = nix-command flakes
      '');
    };

    nixpkgs.config = lib.mkIf config.chvp.nix.enableUnfree (import ./nix/unfree.nix);
    nixpkgs.overlays = lib.mkIf config.chvp.nix.enableFlakes [
      (self: super: {
        nix = super.nixUnstable;
      })
    ];

    home-manager.users.charlotte = { ... }:
      lib.recursiveUpdate
        (lib.optionalAttrs config.chvp.nix.enableDirenv baseDirenv)
        (lib.recursiveUpdate
          (lib.optionalAttrs config.chvp.nix.enableUnfree baseUnfree)
          (lib.optionalAttrs config.chvp.nix.enableNixIndex baseNixIndex));
    home-manager.users.root = { ... }:
      lib.recursiveUpdate
        (lib.optionalAttrs config.chvp.nix.enableDirenv baseDirenv)
        (lib.optionalAttrs config.chvp.nix.enableUnfree baseUnfree);
  };
}
