{ config, lib, ... }:

{
  chvp.zfs.homeLinks = [
    { path = ".config/pipewire"; type = "cache"; }
  ];

  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };
}
