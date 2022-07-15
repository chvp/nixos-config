{ config, lib, pkgs, ... }:

{
  imports = [
    ./bluetooth
    ./emacs
    ./mail
    ./network
    ./nix
    ./smartd
    ./ssh
    ./sshd
    ./telegraf
    ./tmux
    ./zfs
    ./zsh
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
  };

  config = {
    home-manager.useGlobalPkgs = true;

    system = {
      stateVersion = config.chvp.stateVersion;
      autoUpgrade = {
        enable = true;
        flake = "github:chvp/nixos-config";
        dates = "01/4:00";
        randomizedDelaySec = "10min";
      };
    };
    home-manager.users = {
      charlotte = { ... }: {
        home.stateVersion = config.chvp.stateVersion;
        systemd.user.sessionVariables = config.home-manager.users.charlotte.home.sessionVariables;
      };
      root = { ... }: {
        home.stateVersion = config.chvp.stateVersion;
      };
    };

    environment.systemPackages = with pkgs; [ git htop moreutils ncdu ripgrep unzip zip ];

    boot.kernelParams = [ "mitigations=off" ];

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
      supportedLocales = [
        "en_GB.UTF-8/UTF-8"
        "en_IE.UTF-8/UTF-8"
        "en_US.UTF-8/UTF-8"
      ];
    };

    security = {
      sudo.enable = false;
      doas = {
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
    };

    services.fwupd.enable = true;

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users = {
        charlotte = {
          isNormalUser = true;
          home = "/home/charlotte";
          description = "Charlotte Van Petegem";
          extraGroups = [ "systemd-journal" ];
          passwordFile = config.age.secrets."passwords/users/charlotte".path;
        };
        root.passwordFile = config.age.secrets."passwords/users/root".path;
      };
    };

    age.secrets."passwords/users/charlotte".file = ../../secrets/passwords/users/charlotte.age;
    age.secrets."passwords/users/root".file = ../../secrets/passwords/users/root.age;
  };
}
