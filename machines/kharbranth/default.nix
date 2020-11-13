{ config, pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./secret.nix
    ../../configurations/eid.nix
    ../../profiles/bluetooth.nix
    ../../profiles/common.nix
    ../../profiles/graphical.nix
  ];

  networking = {
    hostId = "e718389d";
    hostName = "kharbranth";
  };

  time.timeZone = "Europe/Brussels";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09";

  home-manager.users.charlotte = { ... }: {
    home.stateVersion = "20.09";
  };

  # Machine-specific application settings
  chvp = {
    git.email = "charlotte.vanpetegem@ugent.be";
    zfs = {
      enable = true;
      encrypted = true;
    };
  };
}
