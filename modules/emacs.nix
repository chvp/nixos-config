{ config, lib, pkgs, ... }:

{
  options.chvp.emacs = {
    enable = lib.mkOption {
      default = true;
      example = false;
    };
  };

  config = lib.mkIf config.chvp.emacs.enable {
    home-manager.users.charlotte = { ... }: {
      programs.emacs = {
        enable = true;
        package = pkgs.emacsPgtkGcc;
        init = {
          enable = true;
          prelude = ''
            (when window-system
              (set-frame-font "Fira Code 9"))
            (electric-pair-mode)
            (global-display-line-numbers-mode)

            (defun emoji-fonts ()
                (set-fontset-font t 'symbol "Noto Color Emoji")
                (set-fontset-font t 'symbol "Symbola" nil 'append))
            (if (daemonp)
                (add-hook 'server-after-make-frame-hook #'emoji-fonts)
                    (emoji-fonts))

            ;; Disable default evil keybindings, since evil-collection is a superset
            ;; See https://github.com/emacs-evil/evil-collection/issues/60
            (setq evil-want-keybinding nil)
          '';
          recommendedGcSettings = true;
          usePackageVerbose = true;
          usePackage = {
            better-defaults.enable = true;
            company = {
              enable = true;
              diminish = [ "company-mode" ];
              config = "(global-company-mode)";
            };
            evil = {
              enable = true;
              config = ''
                (evil-mode 1)
              '';
            };
            evil-collection = {
              enable = true;
              after = [ "evil" ];
            };
            fira-code-mode = {
              enable = true;
              config = "(when window-system (global-fira-code-mode))";
            };
            flycheck = {
              enable = true;
              diminish = [ "flycheck-mode" ];
              config = "(global-flycheck-mode)";
            };
            general = {
              enable = true;
              after = [ "evil" "which-key" ];
              config = ''
                (general-evil-setup)
              '';
            };
            ledger-mode = {
              enable = true;
              mode = [ ''"\\.journal\\'"'' ];
              config = ''
                (setq ledger-binary-path "hledger"
                      ledger-highlight-xact-under-point nil
                      ledger-post-account-alignment-column 4
                      ledger-post-amount-alignment-at :decimal
                      ledger-post-amount-alignment-column 59
                      ledger-post-auto-align t)
              '';
            };
            lsp-mode = {
              enable = true;
              command = [ "lsp" ];
            };
            magit = {
              enable = true;
            };
            markdown-mode = {
              enable = true;
              command = [ "markdown-mode" "gfm-mode" ];
              mode = [
                ''("README\\.md\\'" . gfm-mode)''
                ''("\\.md\\'" . markdown-mode)''
                ''("\\.markdown\\'" . markdown-mode)''
              ];
            };
            modus-themes = {
              enable = true;
              config = ''
                (setq modus-themes-bold-constructs t
                      modus-themes-syntax 'alt-syntax-yellow-comments
                      modus-themes-promts 'intense-accented
                      modus-themes-mode-line 'borderless
                      modus-themes-region 'bg-only)
                (modus-themes-load-themes)
                (modus-themes-load-operandi)
              '';
            };
            nix.enable = true;
            nix-mode = {
              enable = true;
              mode = [ ''"\\.nix\\'"'' ];
            };
            python-mode = {
              enable = true;
              mode = [ ''"\\.py\\'"'' ];
            };
            ruby-mode = {
              enable = true;
              mode = [
                ''("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)''
                ''("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)''
              ];
            };
            web-mode = {
              enable = true;
              mode = [ ''"\\.html\\.erb\\'"'' ];
            };
            which-key = {
              enable = true;
              diminish = [ "which-key-mode" ];
              config = "(which-key-mode)";
            };
          };
        };
      };
    };
  };
}
