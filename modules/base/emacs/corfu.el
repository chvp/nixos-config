;; Enable cycling through completions
(setopt corfu-cycle t)
;; Show completion preview by default
(setopt corfu-auto t)
;; Show completion after two characters
(setopt corfu-auto-prefix 2)
;; Space occurs too often in my normal workflow to not quit on no match
(setopt corfu-quit-no-match t)

(require 'corfu)
(require 'corfu-popupinfo)

(global-corfu-mode)
(corfu-popupinfo-mode)
(diminish 'corfu-mode)

(keymap-set corfu-map "RET" nil)
