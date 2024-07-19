(use-package emacs-on-darwin
  :ensure nil ;; Not a real package, but a place to collect global settings for darwin
  :demand t
  :config
  (defun focus-frame ()
    (select-frame-set-input-focus (selected-frame)))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'focus-frame))
  )

