{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.sound.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.sound.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".local/state/wireplumber"; type = "cache"; }
    ];

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [
        pavucontrol
        qjackctl
      ];
    };

    sound.enable = true;
    services = {
      avahi.enable = true;
      pipewire = {
        enable = true;
        alsa.enable = true;
        jack.enable = true;
        pulse.enable = true;
      };
    };
  };
}
