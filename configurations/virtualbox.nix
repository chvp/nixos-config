{ ... }: {
  chvp.zfs.homeLinks = [
    { path = "VirtualBox VMs"; type = "data"; }
  ];

  virtualisation.virtualbox.host.enable = true;

  users.users.charlotte.extraGroups = [ "vboxusers" ];
}
