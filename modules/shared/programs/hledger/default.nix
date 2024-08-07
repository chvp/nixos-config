{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.programs.hledger.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.hledger.enable {
    chvp.base.emacs.extraConfig = [
      ''
        ;; Ledger syntax support
        (use-package ledger-mode
         :mode "\\.journal\\'"
         :custom
         (ledger-binary-path "hledger" "Use hledger instead of ledger")
         (ledger-highlight-xact-under-point nil "Remove distracting highlight")
         (ledger-mode-should-check-version nil "Remove version check, since it doesn't work with hledger anyway")
         (ledger-post-account-alignment-column 4 "Indent postings with 4 spaces")
         (ledger-post-amount-alignment-at :decimal "Align on the decimal")
         (ledger-post-amount-alignment-column 69 "Align on column 70")
         (ledger-post-auto-align t "Align when moving to the next line")
         :config
         (advice-add 'ledger-complete-at-point :around #'cape-wrap-nonexclusive)
         )
      ''
    ];

    home-manager.users.${username}.home.packages = [ pkgs.hledger ];
  };
}
