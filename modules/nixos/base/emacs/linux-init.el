(use-package emacs-on-linux
  :ensure nil ;; Not a real package, but a place to collect global settings for linux
  :demand t
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
  )
