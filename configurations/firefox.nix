{ ... }: {
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ firefox ];
  };

  nixpkgs.overlays = [
    (self: super: {
      firefox = super.firefox.override { extraNativeMessagingHosts = [ self.passff-host ]; pkcs11Modules = [ self.eid-mw ]; };
      # Avoids a double firefox install, see https://github.com/NixOS/nixpkgs/pull/31772
      firefox-bin = self.firefox;
    })
  ];

  custom.zfs.homeLinks = [
    { path = ".cache/mozilla"; type = "cache"; }
    { path = ".mozilla"; type = "data"; }
  ];
}
