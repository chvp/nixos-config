{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "usbhid" "usb_storage" "sd_mod" "sdhci_pci" ];
      kernelModules = [ "i915" ];
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    kernel.sysctl = {
      "vm.swappiness" = 1;
    };
  };

  fileSystems."/" = {
    device = "rpool/local/root";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
  };

  fileSystems."/data" = {
    device = "rpool/safe/data";
    fsType = "zfs";
  };

  fileSystems."/cache" = {
    device = "rpool/local/cache";
    fsType = "zfs";
  };


  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BEEE-D83A";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/6c09b90f-8971-4702-a18a-f06dfb3d8dcd"; }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
    opengl.extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
  };
  services.fstrim.enable = true;
}
