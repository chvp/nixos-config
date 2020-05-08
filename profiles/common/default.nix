{ pkgs, ... }:

{
  imports = [
    ./secret.nix
    ../../configurations/direnv/default.nix
    ../../configurations/git/default.nix
    ../../configurations/neovim/default.nix
    ../../configurations/ssh/default.nix
    ../../configurations/tmux/default.nix
    ../../configurations/zsh/default.nix
  ];

  # Use latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  i18n = {
    defaultLocale = "en_IE.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "en_GB.UTF-8";
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  nix = {
    trustedUsers = [ "@wheel" ];
    gc = {
      automatic = true;
      dates = "hourly";
      options = "--delete-older-than 7d";
    };
    optimise = {
      automatic = true;
      dates = [ "hourly" ];
    };
  };

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      htop
      inotify-tools
      ncdu
      nix-index
      (import ../../programs/pass/default.nix { inherit pkgs; })
      (import ../../programs/ssh/default.nix { inherit pkgs; })
      ripgrep
      unzip
      youtube-dl
    ];
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

  services = {
    atd.enable = true;
    locate = {
      enable = true;
      interval = "hourly";
      localuser = "charlotte";
    };
  };

  system.autoUpgrade = {
    allowReboot = false;
    enable = true;
    dates = "hourly";
  };

  users = {
    mutableUsers = false;
    defaultUserShell = pkgs.zsh;
    users = {
      charlotte = {
        isNormalUser = true;
        home = "/home/charlotte";
        description = "Charlotte Van Petegem";
        extraGroups = [ "wheel" ];
      };
    };
  };
}
