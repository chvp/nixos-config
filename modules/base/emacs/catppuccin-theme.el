(require 'catppuccin-theme)

;; Default is the light theme
(setopt catppuccin-flavor 'latte)
(load-theme 'catppuccin :no-confirm)

(defun chvp--dark-mode ()
  (interactive)
  (progn
    (setq catppuccin-flavor 'frappe)
    (catppuccin-reload)))
(defun chvp--light-mode ()
  (interactive)
  (progn
    (setq catppuccin-flavor 'latte)
    (catppuccin-reload)))
