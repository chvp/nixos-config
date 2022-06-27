{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.terminal.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.terminal.enable {
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = [ pkgs.wezterm ];
      xdg.configFile."wezterm/wezterm.lua".source = ./wezterm.lua;
    };
  };
}
