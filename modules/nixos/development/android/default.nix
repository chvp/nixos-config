{ config, lib, pkgs, ... }:

{
  options.chvp.development.android.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.android.enable {
    chvp.base = {
      nix.unfreePackages = [ "android-studio-stable" ];
      zfs.homeLinks = [
        { path = ".android"; type = "cache"; }
        { path = ".config/Google"; type = "cache"; }
        { path = ".local/share/Google"; type = "cache"; }
        { path = ".cache/Google"; type = "cache"; }
      ];
    };

    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.android-studio ];
    };

    programs.adb.enable = true;
    users.users.charlotte.extraGroups = [ "adbusers" "dialout" "uucp" ];
  };
}
