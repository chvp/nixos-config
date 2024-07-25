(use-package emacs-on-linux
  :ensure nil ;; Not a real package, but a place to collect global settings for linux
  :demand t
  :config
  ;; Make sure DISPLAY is set correctly in env.
  (defun display-env-hack ()
    "Hack DISPLAY env variable back into env."
    (setenv "DISPLAY" ":0")
    )
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'display-env-hack))
  )
