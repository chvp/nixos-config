{ config, lib, pkgs, ... }:

{
  options.chvp.bluetooth.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.bluetooth.enable {
    chvp.zfs.systemLinks = [{ path = "/var/lib/bluetooth"; type = "cache"; }];

    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
    hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
    hardware.pulseaudio.package = pkgs.pulseaudioFull;

    home-manager.users.charlotte = lib.mkIf config.chvp.bluetooth.enable ({ ... }: {
      services.blueman-applet.enable = true;
    });
  };
}
