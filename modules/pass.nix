{ config, lib, ... }:

{
  options.chvp.pass.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.pass.enable {
    nixpkgs.overlays = [
      (self: super: {
        firefox = super.firefox.override { extraNativeMessagingHosts = [ self.passff-host ]; };
        pass = (super.pass.override { pass = super.pass-wayland; }).withExtensions (ext: [ ext.pass-otp ]);
      })
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
