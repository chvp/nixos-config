prog-mode

(fixme (if (or (derived-mode-p 'racket-mode) (derived-mode-p 'emacs-lisp-mode)) ";; " comment-start) "FIXME(chvp): ")
(todo (if (or (derived-mode-p 'racket-mode) (derived-mode-p 'emacs-lisp-mode)) ";; " comment-start) "TODO(chvp): ")
