{ config, lib, pkgs, ... }:

{
  imports = [
    ./default/secret.nix
    ./bluetooth.nix
    ./docker.nix
    ./eid.nix
    ./git.nix
    ./global-mailer.nix
    ./gomuks.nix
    ./neovim.nix
    ./nix.nix
    ./nginx.nix
    ./ovh.nix
    ./smartd.nix
    ./ssh.nix
    ./sshd.nix
    ./syncthing-server.nix
    ./teeworlds.nix
    ./tmux.nix
    ./zeroad.nix
    ./zfs.nix
    ./zsh.nix
  ];

  options.chvp = {
    stateVersion = lib.mkOption {
      example = "20.09";
    };

    dataPrefix = lib.mkOption {
      default = "";
      example = "/data";
    };

    cachePrefix = lib.mkOption {
      default = "";
      example = "/cache";
    };

    graphical = lib.mkOption {
      default = false;
      example = true;
    };

    hasContainers = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = {
    home-manager.useGlobalPkgs = true;

    system.stateVersion = config.chvp.stateVersion;
    home-manager.users = {
      charlotte = { ... }: {
        home.stateVersion = config.chvp.stateVersion;
      };
      root = { ... }: {
        home.stateVersion = config.chvp.stateVersion;
      };
    };

    environment.systemPackages = with pkgs; [
      htop
      ncdu
      ripgrep
    ];

    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    i18n = {
      defaultLocale = "en_IE.UTF-8";
      extraLocaleSettings = {
        LC_TIME = "en_GB.UTF-8";
      };
    };

    networking.nat = lib.mkIf config.chvp.hasContainers {
      enable = true;
      enableIPv6 = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "eno3";
    };

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users = {
        charlotte = {
          isNormalUser = true;
          home = "/home/charlotte";
          description = "Charlotte Van Petegem";
          extraGroups = [ "wheel" "systemd-journal" ] ++ lib.optionals config.chvp.graphical [ "input" "video" ];
        };
      };
    };
  };
}
