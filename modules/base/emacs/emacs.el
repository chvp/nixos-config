(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(add-hook 'text-mode-hook #'electric-pair-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(setopt auto-revert-avoid-polling t
        auto-revert-check-vc-info t
        backup-by-copying t
        column-number-mode t
        completion-ignore-case t
        create-lockfiles nil
        dired-auto-revert-buffer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        fill-column 80
        frame-resize-pixelwise t
        global-auto-revert-mode t
        global-hl-line-sticky-flag 'window
        indent-tabs-mode nil
        horizontal-scroll-bar nil
        inhibit-startup-screen t
        initial-major-mode 'fundamental-mode
        line-number-mode t
        menu-bar-mode nil
        mode-line-compact 'long
        mouse-yank-at-point t
        project-mode-line t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        require-final-newline t
        savehist-mode t
        save-place-mode t
        scroll-bar-mode nil
        search-default-mode t
        sentence-end-double-space nil
        shell-command-prompt-show-cwd t
        show-paren-mode t
        show-paren-context-when-offscreen 'overlay
        tool-bar-mode nil
        treesit-enabled-modes t
        treesit-font-lock-level 3
        use-short-answers t
        view-read-only t
        visible-bell t
        window-combination-resize t
        window-resize-pixelwise t)

(defun chvp--diminish-flyspell-mode ()
  "Diminish `flyspell-mode' when it's enabled."
  (diminish 'flyspell-mode)
  (remove-hook 'flyspell-mode-hook #'chvp--diminish-flyspell-mode)
  )

(defun chvp--diminish-flyspell-prog-mode ()
  "Diminish `flyspell-prog-mode' when it's enabled."
  (diminish 'flyspell-prog-mode)
  (remove-hook 'flyspell-prog-mode-hook #'chvp--diminish-flyspell-prog-mode)
  )

(defun chvp--diminish-hs-minor-mode ()
  "Diminish `hs-minor-mode' when it's enabled."
  (diminish 'hs-minor-mode)
  (remove-hook 'hs-minor-mode-hook #'chvp--diminish-hs-minor-mode)
  )

(add-hook 'flyspell-mode-hook #'chvp--diminish-flyspell-mode)
(add-hook 'flyspell-prog-mode-hook #'chvp--diminish-flyspell-prog-mode)
(add-hook 'hs-minor-mode-hook #'chvp--diminish-hs-minor-mode)
(diminish 'autorevert-mode)
(diminish 'eldoc-mode)

(defun chvp--font-settings ()
  "Setup font settings."
  (when window-system
    (progn (set-frame-font "Hack 9")
           (set-fontset-font t 'symbol "Noto Color Emoji"))))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'chvp--font-settings)
  (chvp--font-settings))

(defun chvp--display-env-hack ()
  "Hack DISPLAY env variable back into env."
  (setenv "DISPLAY" ":0"))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'chvp--display-env-hack))
