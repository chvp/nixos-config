(require 'general)
(general-evil-setup t)

(defun chvp--kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  )

;; Create bindings under the leader
(general-create-definer lmap
  :states '(normal visual insert emacs motion)
  :prefix "SPC"
  :global-prefix "C-SPC"
  )

(nmap "<escape>" '("save current buffer" . save-buffer))
(lmap
  ""     nil ;; Unbind SPC, I don't use it for navigation anyway.

  "SPC"  '(:ignore t :which-key "mode")

  ":"    '("eval" . eval-expression)

  "b"    '(:ignore t :which-key "buffer")
  "bd"   '("kill" . chvp--kill-current-buffer)
  "br"   '("rename" . rename-buffer)

  "f"    '(:ignore t :which-key "file")
  "ff"   '("find" . find-file)
  "fs"   '("save" save-buffer)

  "h"    '(:ignore t :which-key "help")
  "hb"   '("binding" . describe-bindings)
  "hf"   '("function" . describe-function)
  "hv"   '("variable" . describe-variable)

  "q"    '(:ignore t :which-key "quit")
  "qq"   '("quit" . delete-frame)

  "s"    '(:ignore t :which-key "search")

  "w"    '(:ignore t :which-key "window")
  "wv"   '("split vertical" . split-window-vertically)
  "ws"   '("split horizontal" . split-window-horizontally)
  "wd"   '("delete" . delete-window)
  "w-"   '("decrease font size" . text-scale-decrease)
  "w+"   '("increase font size" . text-scale-increase)

  "x"    '("exec" . execute-extended-command)
)
