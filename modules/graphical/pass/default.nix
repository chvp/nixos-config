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

    chvp.base.emacs.extraConfig = [
      ''
        (use-package secrets
         :ensure nil
         :custom
         (auth-sources '(default))
         )
      ''
    ];

    home-manager.users.${username} = {
      home.packages = [ pkgs.keepassxc ];
      systemd.user.services.keepassxc = {
        Unit = {
          Description = "KeepassXC startup";
          PartOf = [ "mango-session.target" ];
          Wants = [ "waybar.service" ];
          After = [
            "mango-session.target"
            "waybar.service"
          ];
        };
        Service = {
          ExecStart = "${pkgs.keepassxc}/bin/keepassxc";
          Restart = "always";
        };
        Install.WantedBy = [ "mango-session.target" ];
      };
    };
  };
}
