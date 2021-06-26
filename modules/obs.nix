{ config, lib, pkgs, ... }:

{
  options.chvp.obs.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.obs.enable {
    boot.kernelModules = [ "v4l2loopback" ];
    boot.extraModulePackages = [ pkgs.linuxPackages.v4l2loopback ];
    boot.extraModprobeConfig = ''
      options v4l2loopback video_nr=9 card_label="obs"
    '';

    chvp.zfs.homeLinks = [
      { path = ".config/obs-studio"; type = "data"; }
    ];

    home-manager.users.charlotte = { pkgs, ... }: {
      programs.obs-studio = {
        enable = true;
        plugins = [ pkgs.obs-studio-plugins.wlrobs ];
      };
    };
  };
}
