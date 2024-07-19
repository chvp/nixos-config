{ config, lib, pkgs, ... }:

{
  home-manager.users.charlotte.programs.git = lib.mkIf config.chvp.development.git.enable {
    extraConfig.tag.gpgSign = config.chvp.graphical.enable;
    signing = {
      key = "charlotte@vanpetegem.me";
      signByDefault = config.chvp.graphical.enable;
    };
  };
}
