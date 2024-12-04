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
    initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" ];
    kernelModules = [ "kvm-amd" ];
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
    "/cache" = {
      device = "zroot/local/cache";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/data" = {
      device = "zroot/safe/data";
      fsType = "zfs";
      neededForBoot = true;
    };
    "/srv/data" = {
      device = "zdata/safe/data";
      fsType = "zfs";
    };
    "/boot/ESP0" = {
      device = "/dev/disk/by-uuid/DA42-87E1";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };
    "/boot/ESP1" = {
      device = "/dev/disk/by-uuid/DA6B-6F76";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };
    "/var/lib/accentor" = {
      device = "zroot/local/services/accentor";
      fsType = "zfs";
    };
    "/var/lib/accentor/transcodes" = {
      device = "zdata/local/services/accentor-transcode-cache";
      fsType = "zfs";
    };
    "/var/lib/docker" = {
      device = "zroot/local/services/docker";
      fsType = "zfs";
    };
    "/var/lib/transmission" = {
      device = "zroot/safe/services/transmission";
      fsType = "zfs";
    };
    "/var/lib/private/gitea-runner" = {
      device = "zroot/local/services/gitea-runner";
      fsType = "zfs";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/5e9ba365-bd67-49c8-9972-b853c759d132"; }
    { device = "/dev/disk/by-uuid/cf8ef6ea-e280-4ae6-bbd4-7018fd573fe4"; }
    { device = "/dev/disk/by-uuid/086a5b7b-09c8-4270-bd72-cc99260c0583"; }
    { device = "/dev/disk/by-uuid/c683d01d-56e9-4dd5-aa9d-6d04864f01a9"; }
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };
  services.fstrim.enable = true;
}
