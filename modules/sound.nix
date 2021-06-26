{ config, lib, pkgs, ... }:

{
  options.chvp.sound.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.sound.enable {
    chvp.zfs.homeLinks = [
      { path = ".config/pipewire"; type = "cache"; }
    ];

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [
        pavucontrol
        qjackctl
      ];
    };

    sound.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      jack.enable = true;
      pulse.enable = true;
    };
  };
}
