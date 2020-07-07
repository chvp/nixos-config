self: super: {
  firefox = super.firefox.override { extraNativeMessagingHosts = [ self.passff-host ]; pkcs11Modules = [ self.eid-mw ]; };
  # Avoids a double firefox install, see https://github.com/NixOS/nixpkgs/pull/31772
  firefox-bin = self.firefox;
}
