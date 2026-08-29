{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.graphical.sound.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.sound.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".local/state/wireplumber"; type = "cache"; }
    ];

    home-manager.users.${username} = { ... }: {
      home.packages = with pkgs; [
        pavucontrol
        qjackctl
      ];
    };

    services = {
      pipewire = {
        enable = true;
        alsa.enable = true;
        jack.enable = true;
        pulse.enable = true;
      };
    };
  };
}
