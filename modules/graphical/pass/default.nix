{
  config,
  lib,
  pkgs,
  ...
}:

let
  username = config.chvp.username;
in
{
  options.chvp.graphical.pass.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.pass.enable {
    chvp.base.zfs.homeLinks = [
      {
        path = ".config/keepassxc";
        type = "data";
      }
      {
        path = ".cache/keepassxc";
        type = "cache";
      }
    ];

    chvp.base.emacs.config.secrets = lib.hm.dag.entryAnywhere {
      elisp = ''
        (setopt auth-sources '(default))
        (require 'secrets)
      '';
    };

    home-manager.users.${username} = {
      home.packages = [ pkgs.chipass ];
      systemd.user.services.chipass = {
        Unit = {
          Description = "ChiPass startup";
          PartOf = [ "tray.target" ];
          Wants = [ "waybar.service" ];
          After = [
            "mango-session.target"
            "waybar.service"
          ];
        };
        Service = {
          ExecStart = "${pkgs.chipass}/bin/ChiPass";
          Restart = "always";
        };
        Install.WantedBy = [ "tray.target" ];
      };
    };
  };
}
