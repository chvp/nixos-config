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
      device = "zroot/safe/cache";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/srv/data" = {
      device = "zdata/data";
      fsType = "zfs";
    };
    "/var/lib/private/gitlab-runner" = {
      device = "zdata/big-apps/gitlab-runner";
      fsType = "zfs";
    };
    "/var/lib/docker" = {
      device = "zdata/big-apps/docker";
      fsType = "zfs";
    };
    "/boot/ESP0" = {
      device = "/dev/disk/by-uuid/6ED1-0638";
      fsType = "vfat";
    };
    "/boot/ESP1" = {
      device = "/dev/disk/by-uuid/6F25-C8B8";
      fsType = "vfat";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/2b90207f-2d08-49aa-8a05-2c98c59224c1"; }
    { device = "/dev/disk/by-uuid/e1a09bfa-9253-44f4-8c02-cf11cbde5320"; }
    { device = "/dev/disk/by-uuid/860a9a86-7882-479c-8be8-f51a5edbf7f7"; }
    { device = "/dev/disk/by-uuid/088f30de-c76d-4843-ac62-8442852b372d"; }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };
  services.fstrim.enable = true;
}
