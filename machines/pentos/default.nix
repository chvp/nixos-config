{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./hardware.nix
    ./secret.nix
    ../../profiles/common/default.nix
    ../../profiles/graphical/default.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "pentos";
  networking.wireless.enable = true;

  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  time.timeZone = "Europe/Brussels";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

  home-manager.users.charlotte = { pkgs, ... }: {
    home = {
      stateVersion = "20.03";
    };
  };

  # Machine-specific application settings
  custom = {
    git.email = "charlotte@vanpetegem.me";
  };

  system = {
    autoUpgrade.enable = true;
  };

}
