{ config, lib, pkgs, ... }:

{
  options.chvp.dropbox.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.dropbox.enable {
    chvp = {
      nix.unfreePackages = [ "dropbox" ];
      zfs.homeLinks = [
        { path = ".dropbox"; type = "cache"; }
        { path = "Dropbox"; type = "data"; }
      ];
    };

    # Avoids a double firefox install, see https://github.com/NixOS/nixpkgs/pull/31772
    nixpkgs.overlays = [ (self: super: { firefox-bin = self.firefox; }) ];

    home-manager.users.charlotte = { pkgs, ... }: {
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
  };
}
