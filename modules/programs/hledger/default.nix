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
  options.chvp.programs.hledger.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.hledger.enable {
    chvp.base.emacs.config.ledger-mode = lib.hm.dag.entryAnywhere {
      packages = epkgs: [ epkgs.ledger-mode ];
      elisp = ''
        (setopt ledger-binary-path "hledger") ;; Use hledger instead of ledger
        (setopt ledger-highlight-xact-under-point nil) ;; Remove distracting highlight
        (setopt ledger-mode-should-check-version nil) ;; Remove version check, since it doesn't work with hledger anyway
        (setopt ledger-post-account-alignment-column 4) ;; Indent postings with 4 spaces
        (setopt ledger-post-amount-alignment-at :decimal) ;; Align on the decimal
        (setopt ledger-post-amount-alignment-column 69) ;; Align on column 70
        (setopt ledger-post-auto-align t) ;; Align when moving to the next line
        (require 'ledger-mode)
        (advice-add 'ledger-complete-at-point :around #'cape-wrap-nonexclusive)
        (add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))

      '';
    };

    home-manager.users.${username}.home.packages = [ pkgs.hledger ];
  };
}
