{ ... }:

{
  virtualisation.docker = {
    enable = true;
    extraOptions = "--data-root /data/var/lib/docker";
    storageDriver = "zfs";
  };

  users.users.charlotte.extraGroups = [
    "docker"
  ];

}
