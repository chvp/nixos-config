(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'text-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(setopt use-short-answers t)
(setopt fill-column 80)
(setopt create-lockfiles nil)
(setopt inhibit-startup-screen t)
(setopt native-comp-jit-compilation nil)

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

(column-number-mode)
