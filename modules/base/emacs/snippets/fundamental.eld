fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(NOW (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))
(yesterday (format-time-string "%Y-%0m-%0d" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "%Y-%0m-%0d" (time-add nil (* 24 60 60))))
