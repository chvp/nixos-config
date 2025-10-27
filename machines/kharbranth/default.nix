{ pkgs, lib, config, ... }:

let
  zfsCompatibleKernelPackages = lib.filterAttrs
    (
      name: kernelPackages:
        (builtins.match "linux_[0-9]+_[0-9]+" name) != null
        && (builtins.tryEval kernelPackages).success
        && (!kernelPackages.${config.boot.zfs.package.kernelModuleAttribute}.meta.broken)
    )
    pkgs.linuxKernel.packages;
  latestKernelPackage = lib.last (
    lib.sort (a: b: (lib.versionOlder a.kernel.version b.kernel.version)) (
      builtins.attrValues zfsCompatibleKernelPackages
    )
  );
in
{
  imports = [ ./hardware.nix ];

  boot.kernelPackages = lib.mkForce latestKernelPackage;
  boot.zfs.package = pkgs.zfs_unstable;

  networking.hostId = "93decfce";

  time.timeZone = "Europe/Brussels";

  # Machine-specific module settings
  chvp = {
    stateVersion = "25.11";
    base = {
      bluetooth.enable = true;
      network.mobile = {
        enable = true;
        wireless-interface = "wlp0s20f3";
        wired-interfaces = {
          "enp0s13f0u1u2".MACAddress = "b8:85:84:a9:7d:02";
        };
      };
      zfs = {
        enable = true;
        encrypted = true;
        backups = [
          {
            path = "rpool/safe/data";
            remotePath = "zdata/recv/kharbranth/safe/data";
            fast = true;
            location = "elendel";
          }
        ];
        rootDataset = "rpool/local/root";
        rootPool = "rpool";
      };
    };
    development = {
      enable = true;
      android.enable = true;
    };
    games.enable = true;
    graphical.enable = true;
    programs = {
      calibre.enable = true;
      eid.enable = true;
      element.enable = true;
      hledger.enable = true;
      slack.enable = true;
      teams.enable = true;
      torrents.enable = true;
    };
  };
}
