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
        server.enable = true;
        settings = {
          main = {
            font = "Hack:size=9";
            dpi-aware = "no";
          };
          bell = {
            urgent = true;
            notify = true;
          };
          scrollback.lines = 10000;
          cursor = {
            blink = true;
            color = "ffffff 777777";
          };
          mouse.hide-when-typing = true;
          colors = {
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
          };
        };
      };
    };
  };
}
