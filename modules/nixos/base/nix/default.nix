{ config, lib, pkgs, ... }:
{
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
  };
  nix.gc.dates = if config.chvp.base.nix.slowGc then "daily" else "hourly";
  programs.command-not-found.enable = false;
}
