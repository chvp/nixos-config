{ config, lib, ... }:

{
  options.chvp.graphical.pass.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.pass.enable {
    nixpkgs.overlays = [
      (self: super: {
        pass = (super.pass.override { pass = super.pass-wayland; }).withExtensions (ext: [ ext.pass-otp ]);
      })
    ];

    chvp.base.emacs.extraConfig = [
      ''
        (use-package auth-source-pass
         :ensure nil
         :custom
         (auth-sources '(password-store))
         (auth-source-pass-filename "${config.home-manager.users.charlotte.programs.password-store.settings.PASSWORD_STORE_DIR}")
         )
      ''
    ];

    home-manager.users.charlotte = { ... }: {
      programs.password-store = {
        enable = true;
        settings = { PASSWORD_STORE_DIR = "/home/charlotte/repos/passwords"; };
      };
      services.password-store-sync.enable = true;
    };
  };
}
