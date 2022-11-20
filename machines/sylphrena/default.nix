{ config, options, lib, pkgs, ... }:

let
  uncompressed-firmware = pkgs.callPackage
    ({ lib, runCommand, buildEnv, firmwareFilesList }:
      runCommand "sdm845-uncompressed-firmware-share"
        {
          firmwareFiles = buildEnv {
            name = "sdm845-uncompressed-firmware";
            paths = firmwareFilesList;
            pathsToLink = [
              "/lib/firmware/qcom/sdm845"
            ];
          };
        } ''
        PS4=" $ "
        (
        set -x
        mkdir -p $out/share/
        ln -s $firmwareFiles/lib/firmware/ $out/share/uncompressed-firmware
        )
      '')
    {
      # We have to borrow the pre `apply`'d list, thus `options...definitions`.
      # This is because the firmware is compressed in `apply` on `hardware.firmware`.
      firmwareFilesList = [ config.mobile.device.firmware ];
    };
in
{
  chvp = {
    stateVersion = "22.11";
    base.emacs.enable = false;
    base.zfs.enable = false;
    base.nix.enableNixIndex = false;
  };

  environment.etc."firmware".source = uncompressed-firmware;
  environment.systemPackages = [ uncompressed-firmware ];

  mobile = {
    adbd.enable = true;
    # Make the system rootfs different enough that mixing stage-1 and stage-2
    # will fail and not have weird unexpected behaviours.
    generatedFilesystems.rootfs = lib.mkDefault {
      label = lib.mkForce "MOBILE_NIXOS";
      id = lib.mkForce "12345678-1324-1234-0000-D00D00000001";
    };
    # Override stage-0 support for this example app.
    # It's only noise, and the current stage-0 is not able to boot anything else
    # than a system it was built for anyway.
    quirks.supportsStage-0 = lib.mkForce false;
  };

  fileSystems = {
    "/" = lib.mkDefault {
      autoResize = lib.mkForce false;
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      neededForBoot = true;
    };
  };

  services.getty.autologinUser = "charlotte";

  # The LVGUI interface can be used with volume keys for selecting
  # and power to activate an option.
  # Without this, logind just powers off :).
  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';

  # Skip a long-running build for the documentation HTML.
  documentation.enable = false;
}
