{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./bluetooth
    ./emacs
    ./mail
    ./network
    ./nix
    ./phone-push
    ./smartd
    ./ssh
    ./sshd
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

  config = {
    system.autoUpgrade = {
      enable = true;
      flake = "git+https://git.chvp.be/chvp/nixos-config";
      dates = "2:00";
      operation = "boot";
      randomizedDelaySec = "30min";
      allowReboot = true;
      persistent = true;
      upgrade = false;
      rebootWindow = {
        lower = "01:00";
        upper = "05:00";
      };
    };

    boot.kernelParams = [ "mitigations=off" ];

    console = {
      colors = [
        "51576d"
        "e78284"
        "a6d189"
        "e5c890"
        "8caaee"
        "f4b8e4"
        "81c8be"
        "b5bfe2"
        "626880"
        "e78284"
        "a6d189"
        "e5c890"
        "8caaee"
        "f4b8e4"
        "81c8be"
        "a5adce"
      ];
      earlySetup = true;
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

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
        ${username} = {
          systemd.user.sessionVariables = config.home-manager.users.${username}.home.sessionVariables;
          home.stateVersion = config.chvp.homeStateVersion;
        };
        root = {
          home.stateVersion = config.chvp.homeStateVersion;
        };
      };
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
            users = [ username ];
            noPass = true;
            cmd = "nix-collect-garbage";
            runAs = "root";
          }
        ];
      };
      polkit.enable = true;
    };

    services.fwupd.enable = true;

    system.stateVersion = config.chvp.systemStateVersion;

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users = {
        ${username} = {
          description = "Charlotte Van Petegem";
          isNormalUser = true;
          home = "/home/${username}";
          extraGroups = [ "systemd-journal" ];
          hashedPasswordFile = config.age.secrets."passwords/users/charlotte".path;
          shell = pkgs.zsh;
        };
        root.hashedPasswordFile = config.age.secrets."passwords/users/root".path;
      };
    };

    age.secrets."passwords/users/charlotte".file = ../../secrets/passwords/users/charlotte.age;
    age.secrets."passwords/users/root".file = ../../secrets/passwords/users/root.age;
  };
}
