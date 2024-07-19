{ config, lib, pkgs, ... }:

{
  imports = [
    ./android
    ./docker
    ./git
  ];

  config = lib.mkIf config.chvp.development.enable {
    chvp.development.docker.enable = lib.mkDefault true;

    users.users.charlotte.extraGroups = [ "dialout" "uucp" ];

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
  };
}
