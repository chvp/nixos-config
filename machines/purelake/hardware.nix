{ config, lib, modulesPath, pkgs, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];     
      kernelModules = [ ];
    };
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-amd" "nvidia" "nvidia_uvm" "nvidia_modeset" "nvidia_drm" ];
    blacklistedKernelModules = [ "nouveau" "amdgpu" ];
    kernelParams = [ "nvidia-drm.fbdev=1" ];
    extraModulePackages = [ ];
    extraModprobeConfig = '' options bluetooth disable_ertm=1 '';
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/18edda29-c8f2-4cb9-82a8-89d049db6c59";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/FE25-6729";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  swapDevices = [ ];

  nixpkgs.hostPlatform = "x86_64-linux";
  hardware = {
    cpu.amd.updateMicrocode = true;
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs.pkgsi686Linux; [ libva ];
    };
    nvidia = {
      modesetting.enable = true;
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };
}
