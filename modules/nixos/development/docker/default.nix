{ config, lib, pkgs, ... }:

{
  options.chvp.development.docker.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.docker.enable {
    virtualisation.docker = {
      enable = true;
      extraOptions = "--data-root ${config.chvp.dataPrefix}/var/lib/docker";
      storageDriver = "zfs";
    };

    environment.systemPackages = [ pkgs.docker-compose ];

    users.users.charlotte.extraGroups = [ "docker" ];
  };
}
