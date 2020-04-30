{ pkgs }:

(pkgs.pass-wayland.override { pass = pkgs.pass-wayland; }).withExtensions (ext: [ ext.pass-otp ext.pass-genphrase ])
