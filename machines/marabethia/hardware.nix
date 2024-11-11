{ lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    loader = {
      grub = {
        enable = true;
        efiSupport = true;
        mirroredBoots = [
          { devices = [ "nodev" ]; path = "/boot/ESP0"; }
          { devices = [ "nodev" ]; path = "/boot/ESP1"; }
        ];
      };
      efi.canTouchEfiVariables = true;
    };
    initrd = {
      availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
    };
    kernelModules = [ "kvm-intel" ];
  };

  fileSystems = {
    "/" = {
      device = "zroot/local/root";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/nix" = {
      device = "zroot/local/nix";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/nix/store" = {
      device = "zroot/local/nix-store";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/data" = {
      device = "zroot/safe/data";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/cache" = {
      device = "zroot/local/cache";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/boot/ESP0" = {
      device = "/dev/disk/by-uuid/1779-70F4";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };
    "/boot/ESP1" = {
      device = "/dev/disk/by-uuid/179D-4050";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };
    "/var/lib/forgejo" = {
      device = "zroot/safe/services/forgejo";
      fsType = "zfs";
    };
    "/var/lib/nextcloud" = {
      device = "zroot/safe/services/nextcloud";
      fsType = "zfs";
    };
    "/var/lib/postgresql" = {
      device = "zroot/safe/services/postgresql";
      fsType = "zfs";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/4662d37a-236f-451c-a2ec-521a19aa0e55"; }
    { device = "/dev/disk/by-uuid/31260b02-0867-4721-bc06-24b415c57f36"; }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };
  services.fstrim.enable = true;
}
