{ config, lib, pkgs, ... }:

{
  imports = [
    ./android
    ./docker
  ];

  config = lib.mkIf config.chvp.development.enable {
    chvp = {
      base.zfs.homeLinks = [{ path = "repos"; type = "cache"; }];
      development.docker.enable = lib.mkDefault true;
    };

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
  };
}
