{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    xdg.mimeApps = {
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

        "x-scheme-handler/mailto" = [ "thunderbird.desktop" ];

        "x-scheme-handler/msteams" = [ "teams.desktop" ];
      };
    };
  };
}
