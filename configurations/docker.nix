{ ... }:

{
  chvp.zfs.systemLinks = [
    { path = "/var/lib/docker"; type = "cache"; }
    { path = "/var/lib/docker/volumes"; type = "data"; }
  ];
  virtualisation.docker.enable = true;
  users.users.charlotte.extraGroups = [
    "docker"
  ];

}
