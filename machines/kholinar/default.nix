{ pkgs, lib, ... }:

{
  imports = [ ./hardware.nix ];

  networking.hostId = "3cc1a4b2";

  time.timeZone = "Europe/Brussels";

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    graphical = true;
    android.enable = true;
    bluetooth.enable = true;
    dropbox.enable = true;
    git.email = "charlotte@vanpetegem.me";
    minecraft.client = true;
    mumble.enable = true;
    obs.enable = true;
    steam.enable = true;
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
  };
}
