{ config, lib, pkgs, ... }:

{
  imports = [
    ./default/secret.nix
    ./accentor.nix
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
    ./zotero.nix
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
      colors = [
        "fbffff"
        "ae5865"
        "4d7f43"
        "906c33"
        "2b7ab2"
        "8f63a2"
        "008483"
        "535c65"
        "6d7782"
        "ae5865"
        "4d7f43"
        "906c33"
        "2b7ab2"
        "8f63a2"
        "008483"
        "434951"
      ];
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

    security.sudo.enable = false;
    security.doas = {
      enable = true;
      extraRules = [
        {
          users = [ "charlotte" ];
          noPass = true;
          cmd = "nix-collect-garbage";
          runAs = "root";
        }
      ];
    };

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users = {
        charlotte = {
          isNormalUser = true;
          home = "/home/charlotte";
          description = "Charlotte Van Petegem";
          extraGroups = [ "systemd-journal" ] ++ lib.optionals config.chvp.graphical [ "input" "video" ];
        };
      };
    };
  };
}
