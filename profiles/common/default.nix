{ pkgs, ... }:

{
  imports = [
    ./secret.nix
    ../../programs/direnv/default.nix
    ../../programs/git/default.nix
    ../../programs/neovim/default.nix
    ../../programs/ssh/default.nix
    ../../programs/tmux/default.nix
    ../../programs/zsh/default.nix
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

  programs.gnupg.agent.enable = true;

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      htop
      inotify-tools
      unzip
    ];
  };

  services.locate = {
    enable = true;
    interval = "hourly";
    localuser = "charlotte";
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
