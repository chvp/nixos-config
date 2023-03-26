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
  :after evil
  :config
  (general-evil-setup t)

  ;; Create bindings under the leader
  (general-create-definer lmap
    :states '(normal visual insert emacs motion)
    :prefix "SPC"
    :global-prefix "C-SPC"
    )

  (nmap "<escape>" 'save-buffer)
  (lmap
    ""     nil ;; Unbind SPC, I don't use it for navigation anyway.

    "SPC"  '(:ignore t :which-key "mode")

    ":"    '(eval-expression :which-key "eval")

    "b"    '(:ignore t :which-key "buffer")
    "bd"   '(kill-this-buffer :which-key "kill")
    "br"   '(rename-buffer :which-key "rename")

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
    "w-"   '(text-scale-decrease :which-key "decrease font size")
    "w+"   '(text-scale-increase :which-key "increase font size")

    "x"    '(execute-extended-command :which-key "exec")
    )
  )

;; Vim keybindings
(use-package evil
  :custom
  (evil-want-keybinding nil "Disable default evil keybindings, since
    evil-collection is a superset. See
    https://github.com/emacs-evil/evil-collection/issues/60.")
  (evil-want-integration t "Also needed for evil-collection")
  :config
  (defalias 'mu4e--main-action-str 'mu4e~main-action-str)
  (defalias 'mu4e--main-view-queue 'mu4e~main-view-queue)
  (evil-mode 1)
  )

;; Vim keybindings in other packages
(use-package evil-collection
  :after (evil)
  :diminish (evil-collection-unimpaired-mode)
  :config (evil-collection-init)
  )

;; Better defaults that aren't defaults for some reason.
(use-package better-defaults
  ;; But don't enable ido-mode...
  :config (ido-mode nil)
  )

;; Autocomplete
(use-package company
  :diminish (company-mode)
  :hook ((text-mode prog-mode) . company-mode)
  :custom
  (company-backends '((company-capf company-yasnippet) (company-dabbrev-code company-keywords) company-dabbrev))
  (company-dabbrev-downcase nil "Don't downcase completions")
  (company-dabbrev-ignore-case t "Change full casing of completion if completion has different case")
  (company-minimum-prefix-length 2 "Start autocompletion after 2 characters")
  )

;; Prescient in company
(use-package company-prescient
  :after (company prescient)
  :config (company-prescient-mode 1)
  )

;; Replacements for emacs built-ins that better integrate with `vertico'.
(use-package consult
  :commands (consult-ripgrep)
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
  :demand t
  :hook
  ;; Always display line numbers for text-based modes
  ((text-mode prog-mode) . display-line-numbers-mode)
  ;; Enable basic auto pairs. Maybe replace this with something more
  ;; advanced later? Look into configuring pairs for frequently used
  ;; major modes.
  ((text-mode prog-mode) . electric-pair-mode)
  ;; Always highlight the current line in text modes
  ((text-mode prog-mode) . hl-line-mode)
  :custom
  (use-short-answers t "Allow short answers on yes/no prompt")
  (fill-column 80 "Fill at column 80 instead of 70")
  (create-lockfiles nil "I'm the only user on my devices and use emacs as a daemon, so don't clutter with lockfiles")
  (inhibit-startup-screen t "Don't show default startup screen")
  (comp-deferred-compilation nil "Don't do native-comp at runtime")
  :config

  ;; Font configuration
  (defun font-settings ()
    "Setup font settings."
    (when window-system (set-frame-font "Hack 9"))
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

  ;; Always display column number in mode line
  (column-number-mode)
  )

;; Linting
(use-package flycheck
  :hook ((text-mode prog-mode) . flycheck-mode)
  :diminish (flycheck-mode)
  )

;; Annotations in selection interface
(use-package marginalia
  :after (vertico)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode)
  :general
  (minibuffer-local-map "M-a" 'marginalia-cycle)
  )

;; Theming
(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t) ;; Use bold accents
  (setq modus-themes-fringes nil) ;; No fringes
  (setq modus-themes-common-palette-overrides
    `(
      ;; Colourful active modeline
      (bg-mode-line-active bg-blue-intense)
      (fg-mode-line-active fg-main)
      ;; Borderless modeline
      (border-mode-line-active unspecified)
      (border-mode-line-inactive unspecified)
      ;; Alt syntax colours
      (builtin magenta)
      (comment yellow-cooler)
      (constant magenta-cooler)
      (docstring magenta-faint)
      (docmarkup green-faint)
      (fnname magenta-warmer)
      (keyword cyan)
      (preprocessor cyan-cooler)
      (string red-cooler)
      (type magenta-cooler)
      (variable blue-warmer)
      (rx-construct magenta-warmer)
      (rx-backslash blue-cooler)
      ;; Colours are nice
      ,@modus-themes-preset-overrides-intense
      )
    )
  (setq modus-themes-region '(bg-only)) ;; Don't lose syntax highlighting in the active region
  (load-theme 'modus-operandi t)
  )

(use-package no-littering
  :custom
  (user-emacs-directory (expand-file-name "~/.cache/emacs/") "Don't put files into .emacs.d")
  :config
  ;; Also make sure auto-save files are saved out-of-tree
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

;; Orderless filtering
(use-package orderless
  :after (vertico)
  :custom
  (completion-styles '(orderless basic) "Use orderless for filtering")
  (orderless-matching-styles '(orderless-regexp orderless-initialism orderless-prefixes) "More matching styles for more flexible matching.")
  :config
  ;; Highlight multiple parts in company matches
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (advice-add 'company-capf--candidates :around #'just-one-face)
  )

;; Org
(use-package org
  :hook
  (org-insert-heading . set-creation-date-heading-property)
  :custom
  (org-directory "~/sync/notes" "Store org journal in synced directory")
  (org-default-notes-file (concat org-directory "/inbox.org") "Capture in inbox by default")
  (org-agenda-files '("~/sync/notes") "Let's say all files can contain events for now")
  :demand t
  :config
  (defun find-file-in-org-directory ()
    (interactive)
    (ido-find-file-in-dir org-directory)
    )
  (defun set-creation-date-heading-property () (org-set-property "CREATED" (format-time-string (org-time-stamp-format t t))))
  :general
  (lmap
    :keymap 'org-mode-map
    "SPC i" '(org-insert-heading :which-key "Insert heading")
    "SPC <" '(org-promote-subtree :which-key "Decrease level")
    "SPC >" '(org-demote-subtree :which-key "Increase level")
    "SPC c" '(orc-clone-subtree-with-time-shift :which-key "Repeat subtree")
    )
  (lmap
    "o a" '(org-agenda :which-key "Agenda")
    "o t" '(org-todo-list :which-key "Todo list")
    "o o" '(find-file-in-org-directory :which-key "Find org file")
    )
  )

;; Sorting when filtering
(use-package prescient
  :custom
  (prescient-aggressive-file-save t "Be aggressive with saving prescient data since we're in daemon mode")
  (prescient-history-length 100000 "Save a lot of history")
  (prescient-frequency-threshold 0.00005 "Save a lot of history")
  :config (prescient-persist-mode 1)
  )

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; List item selection interface
(use-package vertico
  :custom (vertico-count 20 "Allow selector to be a bit higher")
  :config (vertico-mode)
  :diminish (vertico-mode)
  )

;; Prescient integration in vertico
(use-package vertico-prescient
  :after (vertico prescient)
  :custom (vertico-prescient-enable-filtering nil "`orderless' manages the filtering part.")
  :config (vertico-prescient-mode 1))

;; Show keybindings
(use-package which-key
  :diminish (which-key-mode)
  :config (which-key-mode)
  )

