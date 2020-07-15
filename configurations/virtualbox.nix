{ ... }: {
  virtualisation.virtualbox.host = {
    enable = true;
  };
  users.users.charlotte.extraGroups = [ "vboxusers" ];
}
