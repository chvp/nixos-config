{ ... }:

{
  custom.zfs.systemLinks = [
    { path = "/var/lib/pulse"; type = "data"; }
  ];

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    systemWide = true;
  };

  users.users.charlotte.extraGroups = [ "audio" ];
}
