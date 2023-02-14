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
          scrollback.lines = 10000;
          cursor.blink = true;
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
          };
        };
      };
    };
  };
}
