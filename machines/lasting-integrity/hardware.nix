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
    };
    "/nix" = {
      device = "zroot/local/nix";
      fsType = "zfs";
    };
    "/nix/store" = {
      device = "zroot/local/nix-store";
      fsType = "zfs";
    };
    "/data" = {
      device = "zroot/safe/data";
      fsType = "zfs";
    };
    "/cache" = {
      device = "zroot/safe/cache";
      fsType = "zfs";
    };
    "/srv/data" = {
      device = "zdata/data";
      fsType = "zfs";
    };
    "/boot/ESP0" = {
      device = "/dev/disk/by-uuid/BC0C-3065";
      fsType = "vfat";
    };
    "/boot/ESP1" = {
      device = "/dev/disk/by-uuid/BC67-2D0D";
      fsType = "vfat";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/7b9d63e0-5525-4022-9d1a-6c62d52dfb78"; }
    { device = "/dev/disk/by-uuid/2602f9a5-c42a-4514-bc4a-30fbb2c08ee9"; }
    { device = "/dev/disk/by-uuid/0f98f67f-227f-4a03-892d-d2dfd37e39ad"; }
    { device = "/dev/disk/by-uuid/c7bd8b09-45cb-42cd-b355-1a1f2ebde6d4"; }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };
  services.fstrim.enable = true;
}
