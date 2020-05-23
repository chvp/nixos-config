{ pkgs, ... }:

{
  imports = [ ./users/secret.nix ];

  users = {
    mutableUsers = false;
    defaultUserShell = pkgs.zsh;
    users = {
      charlotte = {
        isNormalUser = true;
        home = "/home/charlotte";
        description = "Charlotte Van Petegem";
        extraGroups = [
          "input"
          "systemd-journal"
          "video"
          "wheel"
        ];
      };
    };
  };
}
