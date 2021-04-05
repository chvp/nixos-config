{ config, lib, pkgs, ... }:

{
  imports = [
    ./default/secret.nix
    ./accentor.nix
    ./bluetooth.nix
    ./docker.nix
    ./eid.nix
    ./emacs.nix
    ./git.nix
    ./global-mailer.nix
    ./minecraft.nix
    ./nextcloud.nix
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
        "f8f8f8"
        "a60000"
        "005e00"
        "813e00"
        "0031a9"
        "721045"
        "00538b"
        "282828"
        "ffffff"
        "972500"
        "315b00"
        "70480f"
        "2544bb"
        "8f0075"
        "30517f"
        "000000"
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
