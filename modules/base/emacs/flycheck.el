(setopt flycheck-checker-error-threshold 10000)
(require 'flycheck)

(global-flycheck-annotate-mode)

(add-hook 'prog-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(diminish 'flycheck-mode)
