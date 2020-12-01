{ config, lib, pkgs, ... }:

{
  options.chvp.docker.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.docker.enable {
    virtualisation.docker = {
      enable = true;
      extraOptions = "--data-root ${config.chvp.dataPrefix}/var/lib/docker";
      storageDriver = lib.mkIf config.chvp.zfs.enable "zfs";
    };

    environment.systemPackages = [ pkgs.docker-compose ];

    users.users.charlotte.extraGroups = [ "docker" ];
  };
}
