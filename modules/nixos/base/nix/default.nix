{ config, lib, pkgs, ... }:
{
  config = {
    chvp.base = {
      emacs.extraConfig = [
        ''
          ;; Nix syntax support
          (use-package nix-mode
            :mode "\\.nix\\'"
            )
        ''
      ] ++ lib.optional config.chvp.base.nix.enableDirenv ''
        ;; Direnv integration in emacs.
        (use-package direnv
          :config (direnv-mode)
          )
      '';
      zfs.homeLinks = (lib.optional config.chvp.base.nix.enableDirenv { path = ".local/share/direnv"; type = "cache"; });
    };
    nix.gc.dates = if config.chvp.base.nix.slowGc then "daily" else "hourly";
    programs.command-not-found.enable = false;
  };
}
