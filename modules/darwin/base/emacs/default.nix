{ config, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  chvp.base.emacs.basePackage = pkgs.emacs;
  services.emacs = {
    enable = true;
    package = config.chvp.base.emacs.package;
  };
  home-manager.users.${username} = {
    home.packages = [
      (pkgs.writeShellScriptBin "restart-emacs" ''
        launchctl unload ~/Library/LaunchAgents/org.nixos.emacs.plist
        launchctl load ~/Library/LaunchAgents/org.nixos.emacs.plist
        launchctl start ~/Library/LaunchAgents/org.nixos.emacs.plist
      '')
    ];
  };
}
