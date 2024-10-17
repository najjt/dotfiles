(use-package hydra
  :config

  (defhydra hydra-main (:timeout 4)
    "
  Main Menu
  -----------------------------
  [_t_] Choose Theme
  [_r_] Resize Window
  [_c_] Open Calendar
  [_e_] Eglot Commands
  [_q_] Quit"
    ("t" hydra-theme/body nil :exit t)
    ("r" hydra-window/body nil :exit t)
    ("c" my/custom-open-calendar nil :exit t)
    ("e" hydra-eglot/body nil :exit t)
    ("q" nil nil :exit t)))

;; Choose theme and remember it between sessions
(defhydra hydra-theme (:timeout 4)
  "
  Choose theme
  -----------------------------
  [_s_] Spaceway
  [_e_] EF Melissa Light
  [_v_] Modus Vivendi
  [_q_] Quit"
  ("s" (my/enable-theme 'spaceway) nil)
  ("e" (my/enable-theme 'ef-melissa-light) nil)
  ("v" (my/enable-theme 'modus-vivendi) nil)
  ("q" nil nil :exit t))

(defun my/disable-all-themes ()
  "Disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/enable-theme (theme)
  "Enable the specified THEME and disable all other themes."
  (my/disable-all-themes)
  (load-theme theme t)
  (customize-save-variable 'my-chosen-theme theme))

(defun my/enable-theme-interactive (theme)
  "Interactively enable the specified THEME and disable all other themes."
  (interactive
   (list (completing-read "Choose theme: " (mapcar #'symbol-name (custom-available-themes)))))
  (my/disable-all-themes)
  (load-theme (intern theme) t)
  (customize-save-variable 'my-chosen-theme theme))

;; Remember last used theme between sessions
(add-hook 'after-init-hook
          (lambda ()
            (if (boundp 'my-chosen-theme)
                (my/enable-theme my-chosen-theme)
              (my/enable-theme 'modus-vivendi))))

;; Resize window
(defhydra hydra-window (:timeout 4)
  "
  Resize window
  -----------------------------
  [_h_] Decrease width
  [_j_] Increase height
  [_k_] Decrease height
  [_l_] Increase width
  [_q_] Quit"
  ("h" (window-width-decrease)  nil)
  ("j" (window-height-increase) nil)
  ("k" (window-height-decrease) nil)
  ("l" (window-width-increase)  nil)
  ("q" nil nil :exit t))

;; Resizes the window width based on the input
(defun resize-window-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window width in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!")))
               (message "%s" w)
               (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t)))

;; Resizes the window height based on the input
(defun resize-window-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!")))
               (message "%s" h)
               (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil)))

(defun resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

;; Shorcuts for window resize width and height
(defun window-width-increase ()
  (interactive)
  (resize-window t 5))

(defun window-width-decrease ()
  (interactive)
  (resize-window t -5))

(defun window-height-increase ()
  (interactive)
  (resize-window nil 5))

(defun window-height-decrease ()
  (interactive)
  (resize-window nil -5))

(provide 'ml-hydra)
