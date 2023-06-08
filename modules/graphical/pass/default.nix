{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.pass.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.pass.enable {
    chvp.base.emacs.extraConfig = [
      ''
        (use-package secrets
         :ensure nil
         :custom
         (auth-sources '(default))
         )
      ''
    ];

    home-manager.users.charlotte = { ... }: {
      programs.password-store = {
        enable = true;
        settings = { PASSWORD_STORE_DIR = "/home/charlotte/repos/passwords"; };
      };
      services.password-store-sync.enable = true;
      home.packages = [ pkgs.keepassxc ];
    };
  };
}
