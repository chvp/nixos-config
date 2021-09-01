;;; init --- My emacs init file
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-verbose nil)
  (setq use-package-always-ensure t))

;; Dependencies that inject `:keywords' into `use-package' should be
;; included before all other packages.
;; For :diminish in (use-package). Hides minor modes from the status line.
(use-package diminish)
;; For :general in (use-package). Keybinding management framework.
(use-package general
  :config
  (general-evil-setup t)

  ;; Create bindings under the leader
  (general-create-definer lmap
    :states '(normal visual insert emacs motion)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    )

  (lmap
    ""     nil ;; Unbind SPC, I don't use it for navigation anyway.

    "SPC"  '(:ignore t :which-key "mode")

    ":"    '(eval-expression :which-key "eval")

    "b"    '(:ignore t :which-key "buffer")
    "bd"   '(kill-this-buffer :which-key "kill")

    "f"    '(:ignore t :which-key "file")
    "ff"   '(find-file :which-key "find")
    "fs"   '(save-buffer :which-key "save")

    "h"    '(:ignore t :which-key "help")
    "hb"   '(describe-bindings :which-key "bindings")
    "hf"   '(describe-function :which-key "function")
    "hv"   '(describe-variable :which-key "variable")

    "q"    '(:ignore t :which-key "quit")
    "qq"   '(delete-frame :which-key "quit")

    "s"    '(:ignore t :which-key "search")

    "w"    '(:ignore t :which-key "window")
    "wv"   '(split-window-vertically :which-key "split vertical")
    "ws"   '(split-window-horizontally :which-key "split horizontal")
    "wd"   '(delete-window :which-key "delete")

    "x"    '(execute-extended-command :which-key "exec")
    )
  )

;; Better defaults that aren't defaults for some reason.
(use-package better-defaults
  ;; But don't enable ido-mode...
  :config (ido-mode nil)
  )

;; Autocomplete
(use-package company
  :diminish (company-mode)
  :config (global-company-mode)
  )

;; Prescient in company
(use-package company-prescient
  :config (company-prescient-mode 1)
  )

;; Replacements for emacs built-ins that better integrate with `selectrum'.
(use-package consult
  :demand t
  :custom (consult-project-root-function #'projectile-project-root "Use projectile to determine project roots.")
  :general
  (lmap
    "bb"  '(consult-buffer :which-key "switch")
    "fr"  '(consult-recent-file :which-key "recent")
    "ha"  '(consult-apropos :which-key "apropos")
    "ss"  '(consult-line :which-key "search")
    )
  )

;; General emacs settings
(use-package emacs
  :ensure nil ;; Not a real package, but a place to collect global settings
  :hook
  ;; Always display line numbers for text-based modes
  ((text-mode prog-mode) . display-line-numbers-mode)
  ;; Enable basic auto pairs. Maybe replace this with something more
  ;; advanced later? Look into configuring pairs for frequently used
  ;; major modes.
  ((text-mode prog-mode) . electric-pair-mode)
  :custom
  (inhibit-startup-screen t "Don't show default startup screen")
  :config
  ;; Only ask for y/n, never for yes/no.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Font configuration
  (defun font-settings ()
    "Setup font settings."
    (when window-system (set-frame-font "Fira Code 9"))
    (set-fontset-font t 'symbol "Noto Color Emoji")
    (set-fontset-font t 'symbol "Symbola" nil 'append))
  ;; Make sure DISPLAY is set correctly in env.
  (defun display-env-hack ()
    "Hack DISPLAY env variable back into env."
    (setenv "DISPLAY" ":0")
    )
  (if (daemonp)
      (progn
        (add-hook 'server-after-make-frame-hook #'font-settings)
        (add-hook 'server-after-make-frame-hook #'display-env-hack))
    (font-settings))
  )

;; Vim keybindings
(use-package evil
  :custom
  (evil-want-keybinding nil "Disable default evil keybindings, since
    evil-collection is a superset. See
    https://github.com/emacs-evil/evil-collection/issues/60.")
  (evil-want-integration t "Also needed for evil-collection")
  :config (evil-mode 1)
  )

;; Vim keybindings in other packages
(use-package evil-collection
  :after (evil)
  :config (evil-collection-init)
  )

;; Linting
(use-package flycheck
  :diminish (flycheck-mode)
  :config (global-flycheck-mode)
  )

;; Annotations in selection interface
(use-package marginalia
  :demand t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after (lambda () (selectrum-exhibit 'keep-selected)))
  :general
  (minibuffer-local-map "M-a" 'marginalia-cycle)
  )

;; Theming
(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t "Use bold accents")
  (modus-themes-syntax 'alt-syntax-yellow-comments "Show comments in yellow instead of gray")
  (modus-themes-promts 'intense-accented "Colours are nice")
  (modus-themes-mode-line 'borderless "Thin borders are ugly")
  (modus-themes-region 'bg-only "Don't lose syntax highlighting in the active region")
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi)
  )

;; Orderless filtering
(use-package orderless
  :after (selectrum)
  :custom
  (completion-styles '(orderless) "Use orderless for filtering")
  (orderless-skip-highlighting (lambda () selectrum-is-active) "This and the setting below are performance optimisations.")
  (selectrum-highlight-candidates-function #'orderless-highlight-matches "They make sure only the shown candidates are highlighted.")
  (orderless-matching-styles '(orderless-regexp orderless-initialism orderless-prefixes) "More matching styles for more flexible matching.")
  :config
  ;; Highlight multiple parts in company matches
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (advice-add 'company-capf--candidates :around #'just-one-face)
  )

;; Sorting when filtering
(use-package prescient
  :config
  (prescient-persist-mode 1)
  (run-at-time t 300 'prescient--save)
  )

;; List item selection interface
(use-package selectrum
  :custom (selectrum-max-window-height 20 "Allow selector to be a bit higher")
  :config (selectrum-mode 1)
  :diminish (selectrum-mode)
  )

;; Prescient integration in selectrum
(use-package selectrum-prescient
  :custom (selectrum-prescient-enable-filtering nil "`orderless' manages the filtering part.")
  :config (selectrum-prescient-mode 1))

;; Show keybindings
(use-package which-key
  :diminish (which-key-mode)
  :config (which-key-mode)
  )

