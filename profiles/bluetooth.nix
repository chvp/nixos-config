{ pkgs, ... }:

{
  chvp.zfs.systemLinks = [
    { path = "/var/lib/bluetooth"; type = "cache"; }
  ];

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  home-manager.users.charlotte = { ... }: {
    services.blueman-applet.enable = true;
  };
}
