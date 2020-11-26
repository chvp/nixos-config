{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./secret.nix
  ];

  boot.loader = {
    grub = {
      enable = true;
      efiSupport = true;
      mirroredBoots = [
        { devices = [ "nodev" ]; path = "/boot/ESP0"; }
        { devices = [ "nodev" ]; path = "/boot/ESP1"; }
      ];
    };
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot/EFI";
    };
  };

  time.timeZone = "Europe/Berlin";

  networking = {
    hostName = "lasting-integrity";
    hostId = "b352adfe";
    useDHCP = false;
    interfaces = {
      eno1.useDHCP = false;
      eno2.useDHCP = false;
      eno3.useDHCP = false;
      eno4.useDHCP = false;
    };
  };

  users = {
    mutableUsers = false;
    defaultUserShell = pkgs.zsh;
    users.charlotte = {
      isNormalUser = true;
      extraGroups = [ "wheel" "systemd-journal" ];
    };
  };

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "prohibit-password";

  services.zfs.autoScrub.enable = true;
  services.zfs.trim.enable = true;

  system.stateVersion = "20.09";
}
