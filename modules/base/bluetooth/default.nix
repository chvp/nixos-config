{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.base.bluetooth.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.base.bluetooth.enable {
    chvp.base.zfs.systemLinks = [{ path = "/var/lib/bluetooth"; type = "cache"; }];

    hardware.bluetooth.enable = true;
    services.blueman.enable = true;

    home-manager.users.${username} = lib.mkIf config.chvp.graphical.enable {
      services.blueman-applet.enable = true;
    };
  };
}
