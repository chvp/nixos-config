{ pkgs, ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.kitty ];
    xdg.configFile."kitty/kitty.conf".text = ''
      font_family Fira Code
      font_size 9.0
      disable_ligatures cursor

      background #fbffff
      foreground #535c65
      selection_background #6d7782
      selection_foreground #fbffff
      url_color #906c33
      cursor #434951

      # normal
      color0 #fbffff
      color1 #ae5865
      color2 #4d7f43
      color3 #906c33
      color4 #2b7ab2
      color5 #8f63a2
      color6 #008483
      color7 #535c65

      # bright
      color8 #6d7782
      color9 #ae5865
      color10 #4d7f43
      color11 #906c33
      color12 #2b7ab2
      color13 #8f63a2
      color14 #008483
      color15 #434951

      enable_audio_bell no
      visual_bell_duration 0.25

      remember_window_size no
      initial_window_width 1280
      initial_window_width 720
    '';
  };
}
