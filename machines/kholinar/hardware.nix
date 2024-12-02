{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
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


  chvp.base.nix.unfreePackages = [ "displaylink" ];

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
      wlroots_0_18 = prev.wlroots_0_18.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          (
            final.fetchpatch {
              url = "https://gitlab.freedesktop.org/kennylevinsen/wlroots/-/commit/7e5bf4aef5c61401aaf777bd45cf393c538dac3e.patch";
              hash = "sha256-62C7xtqrPgZm+vpjKyp8OsEyE6yIyf4bgecmILi+Qy4=";
            }
          )
        ];
      });
    })
  ];
}
