;;; init --- My emacs init file
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-verbose t)
  (setq use-package-always-ensure t))

;; For :diminish in (use-package).
(require 'diminish)

(use-package better-defaults)

(use-package company
             :diminish (company-mode)
             :config (global-company-mode)
             )

(use-package evil
             ;; Disable default evil keybindings, since evil-collection is a superset
             ;; See https://github.com/emacs-evil/evil-collection/issues/60
             :custom (evil-want-keybinding nil)
             :config (evil-mode 1)
             )

(use-package evil-collection
             :after (evil)
             )

(use-package fira-code-mode
             :config (when window-system (global-fira-code-mode))
             )

(use-package flycheck
             :diminish (flycheck-mode)
             :config (global-flycheck-mode)
             )

(use-package general
             :after (evil which-key)
             :config
                (general-evil-setup)

                (general-mmap
                  :prefix "SPC"
                  "" nil ;; space is next character by default
                  "b" '(:ignore t :which-key "buffer")
                  "bd" '(kill-this-buffer :which-key "kill")

                  "f" '(:ignore t :which-key "file")
                  "ff" '(find-file :which-key "find")
                  "fs" '(save-buffer :which-key "save")

                  "m" '(:ignore t :which-key "mode")

                  "w" '(:ignore t :which-key "window")
                  "wv" '(split-window-vertically :which-key "split vertical")
                  "ws" '(split-window-horizontally :which-key "split horizontal")
                  "wd" '(delete-window :which-key "delete")

                  "q" '(:ignore t :which-key "quit")
                  "qq" '(save-buffers-kill-emacs :which-key "quit"))
             )

(use-package ledger-mode
             :mode "\\.journal\\'"
             :custom
                (ledger-binary-path "hledger")
                (ledger-highlight-xact-under-point nil)
                (ledger-post-account-alignment-column 4)
                (ledger-post-amount-alignment-at :decimal)
                (ledger-post-amount-alignment-column 59)
                (ledger-post-auto-align t)
             )

(use-package lsp-mode
             :commands (lsp)
             )

(use-package magit)

(use-package markdown-mode
             :commands (markdown-mode gfm-mode)
             :mode ("README\\.md\\'" . gfm-mode)
             :mode ("\\.md\\'" . markdown-mode)
             :mode ("\\.markdown\\'" . markdown-mode)
             )

(use-package modus-themes
             :config
             (setq modus-themes-bold-constructs t
                   modus-themes-syntax 'alt-syntax-yellow-comments
                   modus-themes-promts 'intense-accented
                   modus-themes-mode-line 'borderless
                   modus-themes-region 'bg-only)
             (modus-themes-load-themes)
             (modus-themes-load-operandi)
             )

(use-package nix-mode
             :mode "\\.nix\\'"
             )

(use-package python-mode
             :mode "\\.py\\'"
             )

(use-package web-mode
             :mode "\\.html\\.erb\\'")

(use-package which-key
             :diminish (which-key-mode)
             :config (which-key-mode)
             )

(setq inhibit-startup-screen t)

(when window-system
  (set-frame-font "Fira Code 9"))
(electric-pair-mode)
(global-display-line-numbers-mode)

(defun emoji-fonts ()
  "Setup emoji font priorities."
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-fontset-font t 'symbol "Symbola" nil 'append))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'emoji-fonts)
  (emoji-fonts))

(provide 'init)
;;; init.el ends here
