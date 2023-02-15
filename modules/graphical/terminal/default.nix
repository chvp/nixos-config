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
            foreground = "000000";
            background = "ffffff";
            regular0 = "282828";
            regular1 = "a60000";
            regular2 = "005e00";
            regular3 = "813e00";
            regular4 = "0031a9";
            regular5 = "721045";
            regular6 = "00538b";
            regular7 = "f8f8f8";
            bright0 = "000000";
            bright1 = "972500";
            bright2 = "315b00";
            bright3 = "70480f";
            bright4 = "2544bb";
            bright5 = "8f0075";
            bright6 = "30517f";
            bright7 = "ffffff";
          };
        };
      };
    };
  };
}
