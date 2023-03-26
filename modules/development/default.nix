{ config, lib, ... }:

{
  imports = [
    ./android
    ./docker
    ./git
  ];

  options.chvp.development.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.enable {
    chvp = {
      base.emacs.extraConfig = [
        ''
          ;; Editorconfig
          (use-package editorconfig
            :diminish (editorconfig-mode)
            :config
            (editorconfig-mode 1)
            )

          ;; R syntax support
          (use-package ess
            :init
            (load "ess-autoloads")
            :mode ("\\.r\\'" . ess-r-mode)
            :mode ("\\.R\\'" . ess-r-mode)
            )
          
          ;; Language server support
          (use-package eglot
            :general
            (lmap
              :keymaps 'prog-mode-map
              "SPC s" '(eglot :which-key "Add buffer to eglot")
              "SPC f" '(eglot-format :which-key "Format region")
              "SPC F" '(eglot-format :which-key "Format buffer")
              "SPC r" '(eglot-rename :which-key "Rename symbol")
              "SPC a" '(eglot-code-actions :which-key "Relevant local actions")
              "SPC n" '(flymake-goto-next-error :which-key "Next error")
              "SPC p" '(flymake-goto-prev-error :which-key "Previous error")
              )
             :config
             ;;; eclipse-jdt breaks the spec which in turn breaks code actions
             ;;; This behaviour can't be disabled and needs to be worked around
             (cl-defmethod eglot-execute-command
               (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
               "Eclipse JDT breaks spec and replies with edits as arguments."
               (mapc #'eglot--apply-workspace-edit arguments))
             ;;; eglot replaces company-backends with '(company-capf). I still
             ;;; want company-yasnippet as well though
             (add-to-list 'eglot-stay-out-of "company")
            )

          ;; Snippets
          (use-package yasnippet
            :init
            ;; See https://orgmode.org/org.html#index-yasnippet_002eel
            (defun yas/org-very-safe-expand () (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
            (defun chvp/yas-org-fix () (
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)
            ))
            :hook
            (org-mode . chvp/yas-org-fix)
            ;; This is not very nice, but let's just assume that development machines have my nixos-config checked out
            :custom (yas-snippet-dirs '("/home/charlotte/repos/nixos-config/modules/development/snippets/"))
            :diminish (yas-minor-mode)
            :config
            (yas-global-mode 1)
            )

          ;; Forth syntax support
          (use-package forth-mode
            :mode ("\\.fs\\'" . forth-mode)
            :mode ("\\.fb\\'" . forth-block-mode)
          )
          
          ;; Markdown syntax support
          (use-package markdown-mode
            :commands (markdown-mode gfm-mode)
            :mode ("README\\.md\\'" . gfm-mode)
            :mode ("\\.md\\'" . markdown-mode)
            :mode ("\\.markdown\\'" . markdown-mode)
            )

          ;; Haskell language support
          (use-package haskell-mode
            :mode "\\.hs\\'"
            :config
            (require 'haskell-doc)
            )

          ;; Folding
          (use-package origami
            :hook (prog-mode . origami-mode)
            )

          ;; Python syntax support
          (use-package python-mode
            :mode "\\.py\\'"
            )
          
          ;; Ruby language support
          (use-package ruby-mode
           :ensure nil ;; Included with emacs
           :mode "\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'"
           :mode "\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"
           :custom
           (ruby-insert-encoding-magic-comment nil "Don't insert encoding magic comment")
           )

          ;; Rust language support
          (use-package rust-mode
            :mode "\\.rs\\'"
            )

          ;; TypeScript language support
          (use-package typescript-mode
            :mode "\\.ts\\'"
            )

          ;; Vue language support
          (use-package vue-mode
            :mode "\\.vue\\'"
            )
          
          ;; HTML (and HTML template) support
          (use-package web-mode
            :mode "\\.html\\'"
            :mode "\\.html\\.erb\\'"
            )

          ;; YAML syntax support
          (use-package yaml-mode
            :mode "\\.yml\\'"
            :mode "\\.yaml\\'"
            )
        ''
      ];
      development = {
        docker.enable = lib.mkDefault true;
        git.enable = lib.mkDefault true;
      };
    };

    users.users.charlotte.extraGroups = [ "dialout" "uucp" ];

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
  };
}
