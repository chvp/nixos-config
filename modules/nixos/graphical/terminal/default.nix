{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.terminal.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.terminal.enable {
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = [ pkgs.foot ];
      programs.foot = {
        enable = true;
        settings = {
          main = {
            font = "Hack:size=9";
            dpi-aware = "no";
            initial-color-theme = "light";
          };
          bell = {
            urgent = true;
            notify = true;
          };
          scrollback.lines = 10000;
          cursor.blink = true;
          mouse.hide-when-typing = true;
          colors-dark = {
            cursor = "232634 f2d5cf";
            foreground = "c6d0f5";
            background = "303446";
            regular0 = "51576d";
            regular1 = "e78284";
            regular2 = "a6d189";
            regular3 = "e5c890";
            regular4 = "8caaee";
            regular5 = "f4b8e4";
            regular6 = "81c8be";
            regular7 = "b5bfe2";
            bright0 = "626880";
            bright1 = "e78284";
            bright2 = "a6d189";
            bright3 = "e5c890";
            bright4 = "8caaee";
            bright5 = "f4b8e4";
            bright6 = "81c8be";
            bright7 = "a5adce";
            "16" = "ef9f76";
            "17" = "f2d5cf";
            selection-foreground = "c6d0f5";
            selection-background = "4f5369";
            search-box-no-match = "232634 e78284";
            search-box-match = "c6d0f5 414559";
            jump-labels = "232634 ef9f76";
            urls = "8caaee";
          };
          colors-light = {
            cursor = "eff1f5 dc8a78";
            foreground = "4c4f69";
            background = "eff1f5";
            regular0 = "5c5f77";
            regular1 = "d20f39";
            regular2 = "40a02b";
            regular3 = "df8e1d";
            regular4 = "1e66f5";
            regular5 = "ea76cb";
            regular6 = "179299";
            regular7 = "acb0be";
            bright0 = "6c6f85";
            bright1 = "d20f39";
            bright2 = "40a02b";
            bright3 = "df8e1d";
            bright4 = "1e66f5";
            bright5 = "ea76cb";
            bright6 = "179299";
            bright7 = "bcc0cc";
            "16" = "fe640b";
            "17" = "dc8a78";
            selection-foreground = "4c4f69";
            selection-background = "ccced7";
            search-box-no-match = "dce0e8 d20f39";
            search-box-match = "4c4f69 ccd0da";
            jump-labels = "dce0e8 fe640b";
            urls = "1e66f5";
          };
        };
      };
    };
  };
}
