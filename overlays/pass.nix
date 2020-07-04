self: super: {
  pass = (super.pass-wayland.override { pass = super.pass-wayland; }).withExtensions (ext: [ ext.pass-otp ]);
}
