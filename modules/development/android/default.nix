{
  config,
  lib,
  pkgs,
  ...
}:

let
  username = config.chvp.username;
in
{
  options.chvp.development.android.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.android.enable {
    chvp.base = {
      emacs.config = {
        kotlin-ts-mode = lib.hm.dag.entryAnywhere {
          packages = (epkgs: [ epkgs.kotlin-ts-mode ]);
          elisp = ''
            ;; Kotlin language support
            (require 'kotlin-ts-mode)
            (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
          '';
        };
      };
      zfs.homeLinks = [
        {
          path = ".android";
          type = "cache";
        }
      ];
    };

    environment.systemPackages = with pkgs; [ android-tools ];
    users.users.${username}.extraGroups = [
      "adbusers"
      "dialout"
      "uucp"
    ];
  };
}
