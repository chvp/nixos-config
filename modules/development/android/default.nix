{ config, lib, ... }:

{
  options.chvp.development.android.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.android.enable {
    chvp.base = {
      emacs.extraConfig = [
        ''
          ;; Groovy (gradle) language support
          (use-package groovy-mode
           :mode "\\.gradle\\'")

          ;; Kotlin language support
          (use-package kotlin-mode
           :mode "\\.kt\\'")
        ''
      ];
      zfs.homeLinks = [{ path = ".android"; type = "cache"; }];
    };
    programs.adb.enable = true;
    users.users.charlotte.extraGroups = [ "adbusers" ];
  };
}
