;;; init --- My emacs init file
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-verbose nil)
  (setq use-package-always-ensure t))

;; For :diminish in (use-package).
(require 'diminish)
;; For :general in (use-package). Needs to be included before
;; everything else so `use-package' definitely already knows about
;; `:general'.
(use-package general
  :config
  (general-evil-setup t)

  (nmap
    :prefix "SPC"
    "b"  '(:ignore t :which-key "buffer")
    "bd" '(kill-this-buffer :which-key "kill")

    "f"  '(:ignore t :which-key "file")
    "fs" '(save-buffer :which-key "save")

    "h"  '(:ignore t :which-key "help")

    "m"  '(:ignore t :which-key "mode")

    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-emacs :which-key "quit"))

    "s"  '(:ignore t :which-key "search")

    "w"  '(:ignore t :which-key "window")
    "wv" '(split-window-vertically :which-key "split vertical")
    "ws" '(split-window-horizontally :which-key "split horizontal")
    "wd" '(delete-window :which-key "delete")
  )

;; Better defaults that aren't defaults for some reason.
(use-package better-defaults)

;; Autocomplete
(use-package company
  :diminish (company-mode)
  :config (global-company-mode)
  )

;; Replacements for emacs built-ins that better integrate with `ivy'.
(use-package counsel
  :diminish (counsel-mode)
  :config (counsel-mode 1)
  :general
  (nmap
    :prefix "SPC"
    "SPC" '(counsel-M-x :which-key "execute")
    "bb"  '(counsel-switch-buffer :which-key "switch")
    "ff"  '(counsel-find-file :which-key "find")
    "fr"  '(counsel-recentf :which-key "recent")
    "ha"  '(counsel-apropos :which-key "apropos")
    "hd"  '(counsel-descbinds :which-key "bindings")
    "hf"  '(counsel-describe-function :which-key "function")
    "hv"  '(counsel-describe-variable :which-key "variable")
    )
  )

;; Direnv integration in emacs.
(use-package direnv :config (direnv-mode))

;; Vim keybindings
(use-package evil
  :custom
  ;; Disable default evil keybindings, since evil-collection is a superset
  ;; See https://github.com/emacs-evil/evil-collection/issues/60
  (evil-want-keybinding nil)
  (evil-want-integration t)
  :config (evil-mode 1)
  )

;; Vim keybindings in other packages
(use-package evil-collection
  :after (evil)
  :config (evil-collection-init)
  )

;; Ligatures in GUI mode
;; Should probably switch to ligature.el, but it isn't on MELPA (yet).
(use-package fira-code-mode :config (when window-system (global-fira-code-mode)))

;; Linting integration
(use-package flycheck
  :diminish (flycheck-mode)
  :config (global-flycheck-mode)
  )

;; Autocomplete framework
(use-package ivy
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-height 20)
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  :config (ivy-mode 1)
  :diminish (ivy-mode)
  )

;; Ledger syntax support
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

;; Language server support
(use-package lsp-mode :commands (lsp))

;; Git integration
(use-package magit
  :general
  (nmap
    :prefix "SPC"
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "status")
    )
  )

;; Markdown syntax support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode)
  )

;; Theming
(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-syntax 'alt-syntax-yellow-comments)
  (modus-themes-promts 'intense-accented)
  (modus-themes-mode-line 'borderless)
  (modus-themes-region 'bg-only)
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi)
  )

;; Nix syntax support
(use-package nix-mode :mode "\\.nix\\'")

;; Project management
(use-package projectile
  :after (ripgrep)
  :diminish (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  :config (projectile-mode 1)
  :general
  (nmap
    :prefix "SPC"
    "p"  '(:ignore t :which-key "project")
    "pf" '(projectile-find-file :which-key "find")
    "pp" '(projectile-switch-project :which-key "switch")
    "pr" '(projectile-replace :which-key "replace")
    "ps" '(projectile-ripgrep :which-key "search")
    "p!" '(projectile-run-shell-command-in-root :which-key "command")
    "pt" '(projectile-run-term :which-key "term")
    )
  )

;; Python syntax support
(use-package python-mode :mode "\\.py\\'")

;; Ripgrep support (needed for `projectile-ripgrep')
(use-package ripgrep)

;; `ivy'-integrated `/'
(use-package swiper
  :general
  (nmap
    :prefix "SPC"
    "ss" '(swiper :which-key "search")
    )
  (nmap "/" 'swiper)
  )

;; HTML (and HTML template) support
(use-package web-mode
  :mode "\\.html\\.erb\\'")

;; Show keybindings
(use-package which-key
  :diminish (which-key-mode)
  :config (which-key-mode)
  )

;; YAML syntax support
(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  )

;; Enable basic auto pairs. Maybe replace this with something more
;; advanced later? Look into configuring pairs for frequently used
;; modes.
(electric-pair-mode)

;; Always display line numbers
(global-display-line-numbers-mode)

;; Don't show default startup screen
(setq inhibit-startup-screen t)

;; Font configuration
(when window-system (set-frame-font "Fira Code 9"))
(defun emoji-fonts ()
  "Setup emoji font priorities."
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-fontset-font t 'symbol "Symbola" nil 'append))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'emoji-fonts)
  (emoji-fonts))

(provide 'init)
;;; init.el ends here
