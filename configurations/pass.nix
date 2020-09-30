{ ... }: {
  nixpkgs.overlays = [
    (self: super: {
      pass = (super.pass-wayland.override { pass = super.pass-wayland; }).withExtensions (ext: [ ext.pass-otp ]);
    })
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    programs.password-store = {
      enable = true;
      settings = { PASSWORD_STORE_DIR = "/home/charlotte/repos/passwords"; };
    };
    services.password-store-sync.enable = true;
  };
}
