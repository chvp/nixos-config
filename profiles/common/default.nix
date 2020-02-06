{ pkgs, ... }:

{
  imports = [
    ./secret.nix
    ../../programs/git/default.nix
    ../../programs/ssh/default.nix
    ../../programs/tmux/default.nix
    ../../programs/zsh/default.nix
  ];

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

  nix.trustedUsers = [ "@wheel" ];

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
