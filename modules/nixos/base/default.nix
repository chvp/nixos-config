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
    ./tmux
    ./zfs
    ./zsh
  ];

  config = {
    system.autoUpgrade = {
      enable = true;
      flake = "gitlab:chvp/nixos-config?host=git.chvp.be";
      dates = "01/4:00";
      randomizedDelaySec = "10min";
    };
    home-manager.users = {
      charlotte = { ... }: {
        systemd.user.sessionVariables = config.home-manager.users.charlotte.home.sessionVariables;
      };
      root = { ... }: {
        home.stateVersion = config.chvp.homeStateVersion;
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
      polkit.enable = true;
    };

    services.fwupd.enable = true;

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users = {
        charlotte = {
          isNormalUser = true;
          home = "/home/charlotte";
          extraGroups = [ "systemd-journal" ];
          hashedPasswordFile = config.age.secrets."passwords/users/charlotte".path;
        };
        root.hashedPasswordFile = config.age.secrets."passwords/users/root".path;
      };
    };

    age.secrets."passwords/users/charlotte".file = ../../../secrets/passwords/users/charlotte.age;
    age.secrets."passwords/users/root".file = ../../../secrets/passwords/users/root.age;
  };
}
