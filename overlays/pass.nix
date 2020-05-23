self: super: {
  firefox = super.firefox.override { extraNativeMessagingHosts = [ self.passff-host ]; };
  pass = (super.pass-wayland.override { pass = super.pass-wayland; }).withExtensions (ext: [ ext.pass-otp ]);
}
