{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };
    loader.efi.canTouchEfiVariables = true;
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


  chvp.base = {
    nix.unfreePackages = [ "displaylink" ];
    zfs.systemLinks = [{ path = "/etc/secureboot"; type = "cache"; }];
  };

  # For Secure Boot management
  environment.systemPackages = [ pkgs.sbctl ];

  fileSystems."/" = {
    device = "rpool/local/root";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/nix" = {
    device = "rpool/local/nix";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/data" = {
    device = "rpool/safe/data";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/cache" = {
    device = "rpool/local/cache";
    fsType = "zfs";
    neededForBoot = true;
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
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
    };
  };
  services = {
    fstrim.enable = true;
    xserver.videoDrivers = [ "displaylink" "modesetting" "fbdev" ];
  };
  nixpkgs.overlays = [
    (final: prev: {
      wlroots = prev.wlroots.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          (
            final.fetchpatch {
              url = "https://gitlab.freedesktop.org/wlroots/wlroots/uploads/b4f932e370ed03d88f202191eaf60965/DisplayLink.patch";
              hash = "sha256-1HheLsOSnj4OMiA35QCHkWprTNgAeG2tXrGbaQyUrF4=";
            }
          )
        ];
      });
    })
  ];
  environment.variables.WLR_DRM_DEVICES = "/dev/dri/card0:/dev/dri/card1";
}
