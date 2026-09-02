;; Handy completion-at-point-functions
(require 'cape)
(defun chvp--setup-capfs ()
  (add-hook 'completion-at-point-functions #'tempel-complete -50 t)
  (add-hook 'completion-at-point-functions #'cape-file 10 t)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 15 t)
  (add-hook 'completion-at-point-functions #'cape-line 20 t)
  )

(add-hook 'prog-mode-hook #'chvp--setup-capfs)
(add-hook 'text-mode-hook #'chvp--setup-capfs)
