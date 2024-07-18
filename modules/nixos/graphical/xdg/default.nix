{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.xdg.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.xdg.enable {
    chvp.base.zfs.homeLinks = [
      { path = "desktop"; type = "data"; }
      { path = "documents"; type = "data"; }
      { path = "downloads"; type = "cache"; }
      { path = "music"; type = "data"; }
      { path = "pictures"; type = "cache"; }
      { path = "repos"; type = "cache"; }
      { path = "templates"; type = "data"; }
      { path = "videos"; type = "data"; }
    ];

    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ xdg-user-dirs xdg-utils ];
      xdg = {
        enable = true;
        # Some applications overwrite mimeapps.list with an identical file
        configFile."mimeapps.list".force = true;
        mimeApps = {
          enable = true;
          defaultApplications = {
            "image/png" = [ "org.kde.okular.desktop" ];
            "image/jpg" = [ "org.kde.okular.desktop" ];
            "image/jpeg" = [ "org.kde.okular.desktop" ];
            "application/pdf" = [ "org.kde.okular.desktop" ];

            "text/html" = [ "firefox.desktop" ];
            "x-scheme-handler/about" = [ "firefox.desktop" ];
            "x-scheme-handler/http" = [ "firefox.desktop" ];
            "x-scheme-handler/https" = [ "firefox.desktop" ];
            "x-scheme-handler/unknown" = [ "firefox.desktop" ];

            "x-scheme-handler/msteams" = [ "teams.desktop" ];
          };
        };
        userDirs = {
          enable = true;
          desktop = "\$HOME/desktop";
          documents = "\$HOME/documents";
          download = "\$HOME/downloads";
          music = "\$HOME/music";
          pictures = "\$HOME/pictures";
          publicShare = "\$HOME/desktop";
          templates = "\$HOME/templates";
          videos = "\$HOME/videos";
        };
      };
    };
  };
}
