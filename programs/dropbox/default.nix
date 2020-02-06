{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.dropbox-cli ];
    systemd.user.services = {
      dropbox = {
        Unit = {
          Description = "Dropbox";
        };
        Service = {
          Environment = "QT_PLUGIN_PATH=\"/run/current-system/sw/${pkgs.qt5.qtbase.qtPluginPrefix}\" QML2_IMPORT_PATH=\"/run/current-system/sw/${pkgs.qt5.qtbase.qtQmlPrefix}\"";
          ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
          ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
          KillMode = "control-group";
          Restart = "on-failure";
          PrivateTmp = true;
          ProtectSystem = "full";
          Nice = 10;
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
}
