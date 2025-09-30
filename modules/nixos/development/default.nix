{ config, lib, pkgs, ... }:

{
  imports = [
    ./android
    ./docker
  ];

  config = lib.mkIf config.chvp.development.enable {
    chvp = {
      base = {
        nix.unfreePackages = [ "ruby-mine" ];
        zfs.homeLinks = [{ path = "repos"; type = "cache"; }];
      };
      development.docker.enable = lib.mkDefault true;
    };

    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.jetbrains.ruby-mine ];
    };

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;
  };
}
