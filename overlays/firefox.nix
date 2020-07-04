self: super: {
  firefox = super.firefox.override { extraNativeMessagingHosts = [ self.passff-host ]; pkcs11Modules = [ self.eid-mw ]; };
}
