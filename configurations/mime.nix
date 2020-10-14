{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    # Some applications overwrite mimeapps.list with an identical file
    xdg.configFile."mimeapps.list".force = true;
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "image/png" = [ "org.kde.okular.desktop" ];
        "image/jpg" = [ "org.kde.okular.desktop" ];
        "image/jpeg" = [ "org.kde.okular.desktop" ];
        "application/pdf" = [ "org.kde.okular.desktop" ];

        "text/html" = [ "org.qutebrowser.qutebrowser.desktop" ];
        "x-scheme-handler/about" = [ "org.qutebrowser.qutebrowser.desktop" ];
        "x-scheme-handler/http" = [ "org.qutebrowser.qutebrowser.desktop" ];
        "x-scheme-handler/https" = [ "org.qutebrowser.qutebrowser.desktop" ];
        "x-scheme-handler/unknown" = [ "org.qutebrowser.qutebrowser.desktop" ];

        "x-scheme-handler/msteams" = [ "teams.desktop" ];
      };
    };
  };
}
