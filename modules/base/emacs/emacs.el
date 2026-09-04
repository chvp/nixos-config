(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'text-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(setopt backup-by-copying t
        column-number-mode t
        completion-ignore-case t
        create-lockfiles nil
        dired-auto-revert-buffer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        fill-column 80
        frame-resize-pixelwise t
        indent-tabs-mode nil
        horizontal-scroll-bar nil
        inhibit-startup-screen t
        menu-bar-mode nil
        mode-line-compact 'long
        mouse-yank-at-point t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        require-final-newline t
        savehist-mode t
        save-place-mode t
        scroll-bar-mode nil
        search-default-mode t
        shell-command-prompt-show-cwd t
        show-paren-mode t
        tool-bar-mode nil
        use-short-answers t
        view-read-only t
        visible-bell t
        window-resize-pixelwise t)

(diminish 'auto-revert-mode)
(diminish 'flyspell-mode)
(diminish 'flyspell-prog-mode)

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
