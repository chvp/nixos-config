{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  chvp.base.emacs = {
    basePackage = pkgs.emacs-pgtk;
  };
  home-manager.users.${username} = { ... }: {
    services.emacs = {
      enable = true;
      client.enable = true;
      socketActivation.enable = true;
      package = config.chvp.base.emacs.package;
    };
  };
}
