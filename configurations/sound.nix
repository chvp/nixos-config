{ config, lib, ... }:

{
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
  };

  # PulseAudio doesn't play nice with symlinks
  systemd.user.services.pulseaudio.environment = lib.mkIf config.custom.zfs.enable {
    XDG_CONFIG_HOME = "/data/home/charlotte/.config";
  };

  users.users.charlotte.extraGroups = [ "audio" ];
}
