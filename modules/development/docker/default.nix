{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.development.docker.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.docker.enable {
    virtualisation.docker = {
      enable = true;
      extraOptions = "--data-root ${config.chvp.cachePrefix}/var/lib/docker";
      storageDriver = "overlay2";
    };

    environment.systemPackages = [ pkgs.docker-compose ];

    users.users.${username}.extraGroups = [ "docker" ];
  };
}
