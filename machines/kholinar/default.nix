{ pkgs, lib, ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/graphical.nix
  ];

  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModulePackages = [ pkgs.linuxPackages.v4l2loopback ];
  boot.extraModprobeConfig = ''
    options v4l2loopback video_nr=9 card_label="obs"
  '';

  home-manager.users.charlotte = { pkgs, ... }: {
    programs.obs-studio = {
      enable = true;
      package = pkgs.wrapOBS {
        plugins = [ pkgs.obs-studio-plugins.wlrobs ];
      };
    };
  };

  networking.hostId = "3cc1a4b2";

  time.timeZone = "Europe/Brussels";

  # Machine-specific settings
  chvp = {
    stateVersion = "20.09";
    graphical = true;
    bluetooth.enable = true;
    docker.enable = true;
    eid.enable = true;
    git.email = "charlotte@vanpetegem.me";
    minecraft.client = true;
    sshd.enable = true;
    zeroad.enable = true;
    zfs = {
      enable = true;
      encrypted = true;
      backups = [
        {
          path = "rpool/safe/data";
          remotePath = "zdata/recv/kholinar/safe/data";
          fast = true;
          location = "lasting-integrity";
        }
      ];
      rootDataset = "rpool/local/root";
    };
    zotero.enable = true;
  };
}
