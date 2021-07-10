{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.terminal.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.terminal.enable {
    home-manager.users.charlotte = { pkgs, ... }: {
      programs.kitty = {
        enable = true;
        settings = {
          font_family = "Fira Code";
          font_size = 9;
          disable_ligatures = "cursor";
          background = "#ffffff";
          foreground = "#000000";
          cursor = "#777777";
          url_color = "#0031a9";
          # black
          color0 = "#282828";
          color8 = "#000000";
          # red
          color1 = "#a60000";
          color9 = "#972500";
          # green
          color2 = "#005e00";
          color10 = "#315b00";
          # yellow
          color3 = "#813e00";
          color11 = "#70480f";
          # blue
          color4 = "#0031a9";
          color12 = "#2544bb";
          # magenta
          color5 = "#721045";
          color13 = "#8f0075";
          # cyan
          color6 = "#00538b";
          color14 = "#30517f";
          # white
          color7 = "#f8f8f8";
          color15 = "#ffffff";
          enable_audio_bell = false;
          visual_bell_duration = "0.25";
          remember_window_size = false;
        };
      };
    };
  };
}
