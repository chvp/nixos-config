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

;; Direnv integration in emacs.
(use-package direnv :config (direnv-mode))

;; Editorconfig
(use-package editorconfig
  :diminish (editorconfig-mode)
  :custom (editorconfig-get-properties-function 'editorconfig-get-properties)
  :config (editorconfig-mode 1)
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
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'font-settings)
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

;; Ligatures in GUI mode
;; Should probably switch to ligature.el, but it isn't on MELPA (yet).
(use-package fira-code-mode :config (when window-system (global-fira-code-mode)))

;; Linting
(use-package flycheck
  :diminish (flycheck-mode)
  :config (global-flycheck-mode)
  )

;; Haskell language support
(use-package haskell-mode
  :mode "\\.hs\\'")

;; Ledger syntax support
(use-package ledger-mode
  :mode "\\.journal\\'"
  :custom
  (ledger-binary-path "hledger" "Use hledger instead of ledger")
  (ledger-highlight-xact-under-point nil "Remove distracting highlight")
  (ledger-mode-should-check-version nil "Remove version check, since it doesn't work with hledger anyway")
  (ledger-post-account-alignment-column 4 "Indent postings with 4 spaces")
  (ledger-post-amount-alignment-at :decimal "Align on the decimal")
  (ledger-post-amount-alignment-column 59 "Align on column 60")
  (ledger-post-auto-align t "Align when moving to the next line")
  )

;; Language server support
(use-package lsp-mode :commands (lsp))

;; Git integration
(use-package magit
  :demand t
  :general
  (lmap
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "status")
    )
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
  (modus-themes-bold-constructs t "Use bold accents")
  (modus-themes-syntax 'alt-syntax-yellow-comments "Show comments in yellow instead of gray")
  (modus-themes-promts 'intense-accented "Colours are nice")
  (modus-themes-mode-line 'borderless "Thin borders are ugly")
  (modus-themes-region 'bg-only "Don't lose syntax highlighting in the active region")
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi)
  )

;; Mail user agent
(use-package mu4e
  ;; Use mu4e included in the mu package, see emacs.nix
  :ensure nil
  :demand t
  :after (selectrum)
  :hook
  (mu4e-view-mode . display-line-numbers-mode)
  (mu4e-compose-mode . mail/auto-dodona-cc-reply-to)
  :custom
  (mu4e-change-filenames-when-moving t "Avoid sync issues with mbsync")
  (mu4e-maildir "/home/charlotte/mail" "Root of the maildir hierarchy")
  (mu4e-context-policy 'pick-first "Use the first mail context in the list")
  (mu4e-attachment-dir "/home/charlotte/downloads" "Save attachments to downloads folder")
  (mu4e-compose-dont-reply-to-self t "Don't reply to myself on reply to all")
  (mu4e-confirm-quit nil "Don't confirm when quitting")
  (mu4e-completing-read-function 'completing-read "Use default completing read function")
  (mu4e-headers-include-related nil "Don't show related messages by default")
  (mu4e-headers-skip-duplicates nil "Show duplicate emails")
  (message-kill-buffer-on-exit t "Close buffer when finished with email")
  (mm-verify-option 'known "Always verify PGP signatures (known protocols)")
  (mm-discouraged-alternatives '("text/html" "text/richtext") "Discourage showing HTML views")
  (gnus-buttonized-mime-types '("multipart/signed") "Make sure signature verification is always shown")
  (sendmail-program "msmtp" "Use msmtp to send email")
  (message-sendmail-f-is-evil t "Remove username from the emacs message")
  (message-send-mail-function 'message-send-mail-with-sendmail "Use sendmail to send mail instead internal smtp")
  (message-cite-reply-position 'below "Bottom posting is the correct way to reply to email")
  :config
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Personal"
          :match-func (lambda (msg) (when msg (string-prefix-p "/personal/" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-mail-address . "charlotte@vanpetegem.me")
                  (user-full-name . "Charlotte Van Petegem")
                  (mu4e-drafts-folder . "/personal/Drafts")
                  (mu4e-sent-folder . "/personal/INBOX")
                  (mu4e-refile-folder . "/personal/Archive")
                  (mu4e-trash-folder . "/personal/Trash")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "personal"))
                  )
          )
         (make-mu4e-context
          :name "Work"
          :match-func (lambda (msg) (when msg (string-prefix-p "/work/" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-mail-address . "charlotte.vanpetegem@ugent.be")
                  (user-full-name . "Charlotte Van Petegem")
                  (mu4e-drafts-folder . "/work/Drafts")
                  (mu4e-sent-folder . "/work/INBOX")
                  (mu4e-refile-folder . "/work/Archive")
                  (mu4e-trash-folder . "/work/Deleted Items")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "work"))
                  )
          )
         (make-mu4e-context
          :name "Work AAP-WE-FR"
          :match-func (lambda (msg) (when msg (string-prefix-p "/work-aap-we-fr/" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-mail-address . "aap-we-fr@ugent.be")
                  (user-full-name . "Charlotte Van Petegem")
                  (mu4e-drafts-folder . "/work-aap-we-fr/Concepten")
                  (mu4e-sent-folder . "/work-aap-we-fr/Verzonden items")
                  (mu4e-refile-folder . "/work-aap-we-fr/Archief")
                  (mu4e-trash-folder . "/work-aap-we-fr/Verwijderde items")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "work-aap-we-fr"))
                  )
          )
         (make-mu4e-context
          :name "Posteo"
          :match-func (lambda (msg) (when msg (string-prefix-p "/posteo/" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-mail-address . "chvp@posteo.net")
                  (user-full-name . "Charlotte Van Petegem")
                  (mu4e-drafts-folder . "/posteo/Drafts")
                  (mu4e-sent-folder . "/posteo/INBOX")
                  (mu4e-refile-folder . "/posteo/Archive")
                  (mu4e-trash-folder . "/posteo/Trash")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "posteo"))
                  )
          )
         (make-mu4e-context
          :name "Jong Groen"
          :match-func (lambda (msg) (when msg (string-prefix-p "/jonggroen/" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-mail-address . "charlotte@jonggroen.be")
                  (user-full-name . "Charlotte Van Petegem")
                  (mu4e-drafts-folder . "/jonggroen/[Gmail].Drafts")
                  (mu4e-sent-folder . "/jonggroen/INBOX")
                  (mu4e-refile-folder . "/jonggroen/Archive")
                  (mu4e-trash-folder . "/jonggroen/[Gmail].Bin")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "jonggroen"))
                  )
          )
         (make-mu4e-context
          :name "Postbot"
          :match-func (lambda (msg) (when msg (string-prefix-p "/postbot/" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-mail-address . "postbot@vanpetegem.me")
                  (user-full-name . "Charlotte Van Petegem")
                  (mu4e-drafts-folder . "/postbot/Drafts")
                  (mu4e-sent-folder . "/postbot/INBOX")
                  (mu4e-refile-folder . "/postbot/Archive")
                  (mu4e-trash-folder . "/postbot/Trash")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "postbot"))
                  )
          )
         (make-mu4e-context
          :name "Webmaster"
          :match-func (lambda (msg) (when msg (string-prefix-p "/webmaster/" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-mail-address . "webmaster@vanpetegem.me")
                  (user-full-name . "Webmaster")
                  (mu4e-drafts-folder . "/webmaster/Drafts")
                  (mu4e-sent-folder . "/webmaster/INBOX")
                  (mu4e-refile-folder . "/webmaster/Archive")
                  (mu4e-trash-folder . "/webmaster/Trash")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "webmaster"))
                  )
          )
         )
        )
  (add-to-list
   'mu4e-bookmarks
   '(:name "Combined inbox" :query "maildir:/personal/INBOX or maildir:/work/INBOX or maildir:/posteo/INBOX or maildir:/jonggroen/INBOX" :key ?i)
   )
  (define-advice mu4e~context-ask-user
      (:around (orig-fun &rest args) mu4e~context-ask-user-completing-read)
    "Replace `mu4e-read-option` by general-purpose completing-read"
    (cl-letf (((symbol-function 'mu4e-read-option)
               (lambda (prompt options)
                 (let* ((prompt (mu4e-format "%s" prompt))
                        (choice (completing-read prompt (cl-mapcar #'car options) nil t))
                        (chosen-el (cl-find-if (lambda (option) (equal choice (car option))) options)))
                   (if chosen-el
                       (cdr chosen-el)
                     (mu4e-warn "Unknown option: '%s'" choice))))))
      (apply orig-fun args)))
  (define-skeleton mail/dodona-teacher-reply-skeleton
    "Inserts a typical reply when someone uses the general form for a Dodona teacher request."
    "Naam leerkracht: "
    "Dag " str ",\n"
    "\n"
    _
    "\n"
    "Welkom op Dodona! Zou je het volgende formulier kunnen invullen?\n"
    "\n"
    "https://dodona.ugent.be/rights_requests/new/\n"
    "\n"
    "Zo hebben we meteen alle info die we nodig hebben om je "
    "lesgeversrechten te geven op Dodona.\n"
    "\n"
    "Met vriendelijke groeten,\n"
    "Charlotte Van Petegem"
    )
  (defun mail/dodona-cc-reply-to ()
    "Add dodona@ugent.be in cc and reply-to headers."
    (interactive)
    (save-excursion (message-add-header "Cc: dodona@ugent.be\nReply-To: dodona@ugent.be\n"))
    )
  (defun mail/auto-dodona-cc-reply-to ()
    "Set dodona@ugent.be in CC and Reply-To headers when message was directed to dodona@ugent.be"
    (let ((msg mu4e-compose-parent-message))
      (when (and msg (mu4e-message-contact-field-matches msg :to "dodona@ugent.be")) (mail/dodona-cc-reply-to))
      )
    )
  :general
  (lmap "m" '(mu4e :which-key "mail"))
  ;; Unmap SPC in the mail view so we can still use the leader.
  (lmap mu4e-view-mode-map "" nil)
  (lmap mu4e-compose-mode-map
    "SPC s" '(mml-secure-message-sign-pgpmime :which-key "Sign")
    "SPC c" '(mml-secure-message-encrypt-pgpmime :which-key "Encrypt")
    "SPC t" '(mail/dodona-teacher-reply-skeleton :which-key "Teacher rights reply")
    "SPC d" '(mail/dodona-cc-reply-to :which-key "Dodona support headers")
    )
  )

;; Nix syntax support
(use-package nix-mode :mode "\\.nix\\'")

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
  :config (prescient-persist-mode 1)
  )

;; Project management
(use-package projectile
  :after (ripgrep selectrum)
  :demand t
  :diminish (projectile-mode)
  :config (projectile-mode 1)
  :general
  (lmap
    "p"  '(:ignore t :which-key "project")
    "pf" '(projectile-find-file :which-key "find")
    "pp" '(projectile-switch-project :which-key "switch")
    "pr" '(projectile-replace :which-key "replace")
    "ps" '(consult-ripgrep :search "incsearch")
    "pS" '(projectile-ripgrep :which-key "search")
    "p!" '(projectile-run-shell-command-in-root :which-key "command")
    "p&" '(projectile-run-async-shell-command-in-root :which-key "task")
    )
  )

;; Python syntax support
(use-package python-mode :mode "\\.py\\'")

;; Ruby language support
(use-package ruby-mode
  :ensure nil ;; Included with emacs
  :custom
  (ruby-insert-encoding-magic-comment nil "Don't insert encoding magic comment")
  )

;; Ripgrep support (needed for `projectile-ripgrep')
(use-package ripgrep)

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

;; TypeScript language support
(use-package typescript-mode
  :mode "\\.ts'")

;; HTML (and HTML template) support
(use-package web-mode
  :mode "\\.html'"
  :mode "\\.html\\.erb\\'"
  )

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


(provide 'init)
;;; init.el ends here
