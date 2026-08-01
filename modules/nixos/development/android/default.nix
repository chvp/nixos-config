{ config, lib, pkgs, ... }:

{
  options.chvp.development.android.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.android.enable {
    chvp.base = {
      emacs.extraConfig = [
        ''
          ;; Kotlin language support
          (use-package kotlin-ts-mode :mode "\\.kt\\'")
        ''
      ];
      zfs.homeLinks = [{ path = ".android"; type = "cache"; }];
    };

    environment.systemPackages = with pkgs; [ android-tools ];
    users.users.charlotte.extraGroups = [ "adbusers" "dialout" "uucp" ];
  };
}
