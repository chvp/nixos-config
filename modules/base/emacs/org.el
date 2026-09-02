(setopt org-directory "~/sync/Notes")
(setopt org-default-notes-file (concat org-directory "/inbox.org"))

(require 'org)

(defun chvp--find-file-in-org-directory ()
  "Find a file in the org directory."
  (interactive)
  (ido-find-file-in-dir org-directory))

(defun chvp--set-creation-date-heading-property ()
  "Set the CREATED header of an org heading."
  (org-set-property "CREATED" (format-time-string (org-time-stamp-format t t))))

(add-hook 'org-insert-heading-hook #'chvp--set-creation-date-heading-property)

(lmap
  :keymaps 'org-mode-map
  "SPC a" '("Archive subtree" . org-archive-subtree)
  "SPC i" '("Insert heading" . org-insert-heading)
  "SPC <" '("Decrease level" . org-promote-subtree)
  "SPC >" '("Increase level" . org-demote-subtree)
  "SPC c" '("Repeat subtree" . orc-clone-subtree-with-time-shift)
  "SPC x" '("Cut subtree" . org-cut-subtree)
  "SPC p" '("Paste subtree" . org-paste-subtree)
  "SPC t" '("Cycle todo state" . org-todo)
  )

(lmap
  "o"  '(:ignore t :which-key "org")
  "oo" '("find file" . chvp--find-file-in-org-directory)
  )
