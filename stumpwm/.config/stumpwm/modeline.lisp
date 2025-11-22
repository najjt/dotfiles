;; -*-lisp-*-

(setf *time-modeline-string* "%A %d/%m/%y %R"
      *group-format* "%t"
      *window-format* "%n: %30t"
      cpu::*cpu-modeline-fmt* "%c"
      stumptray:*tray-placeholder-pixels-per-space* 8
      *mode-line-timeout* 2
      *mode-line-position*           :bottom
      *mode-line-foreground-color*   "#CECECE"
      *mode-line-border-width*       1
      *mode-line-pad-x*              5
      *mode-line-pad-x*              2)

(setf *screen-mode-line-format*
      (list
       "[%n] | "
       "%v"
       "^>"
       " | %C"
       " | BAT: %B"
       " | %d"
       " | %T"))

;;; Start the mode line
(dolist (h (screen-heads (current-screen)))
  (enable-mode-line (current-screen) h t))
