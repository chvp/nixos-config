{ config, pkgs, ... }:

let
  findImport = (import ../../lib.nix).findImport;
  nixos-hardware = findImport "nixos-hardware";
in
{
  imports = [
    "${nixos-hardware}/common/cpu/intel"
    "${nixos-hardware}/common/pc/laptop"
    "${nixos-hardware}/common/pc/laptop/ssd"
    ./hardware.nix
    ./secret.nix
    ../../configurations/eid.nix
    ../../profiles/bluetooth.nix
    ../../profiles/common.nix
    ../../profiles/graphical.nix
  ];

  networking.hostName = "oldtown";

  time.timeZone = "Europe/Brussels";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03";

  home-manager.users.charlotte = { ... }: {
    home.stateVersion = "20.03";
  };

  # Machine-specific application settings
  custom = {
    git.email = "charlotte.vanpetegem@ugent.be";
  };
}
