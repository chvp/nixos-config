{ config, lib, pkgs, ... }:

{
  imports = [
    ./emacs
    ./nix
    ./phone-push
    ./tmux
    ./zfs
    ./zsh
  ];

  options.chvp = {
    cachePrefix = lib.mkOption {
      default = "";
      example = "/cache";
    };
    dataPrefix = lib.mkOption {
      default = "";
      example = "/data";
    };
    stateVersion = lib.mkOption {
      example = "20.09";
    };
    homeStateVersion = lib.mkOption {
      default = config.chvp.stateVersion;
    };
    systemStateVersion = lib.mkOption {
      default = config.chvp.stateVersion;
    };
    username = lib.mkOption {
      default = "charlotte";
      example = "charlotte.vanpetegem";
    };
  };

  config =
    let
      username = config.chvp.username;
    in
    {
      environment.systemPackages = with pkgs; [
        coreutils
        git
        htop
        moreutils
        ncdu
        ripgrep
        unzip
        zip
      ];

      home-manager = {
        useGlobalPkgs = true;
        users = {
          "${username}" = { ... }: {
            home.stateVersion = config.chvp.homeStateVersion;
          };
        };
      };

      system.stateVersion = config.chvp.systemStateVersion;

      users.users.${username} = {
        description = "Charlotte Van Petegem";
        shell = pkgs.zsh;
      };
    };
}
