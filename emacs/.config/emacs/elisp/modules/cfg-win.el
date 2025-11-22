;; -*- lexical-binding: t; -*-

;; Undo and redo changes in window layout
(winner-mode 1)

;; Make switching buffers more consistent
(setopt switch-to-buffer-obey-display-actions t
        switch-to-buffer-in-dedicated-window 'pop)

;; Manage windows
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom (aw-scope 'frame)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Automatically switch focus to new window when it is created
(defun my/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'my/split-and-follow-horizontally)

(defun my/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'my/split-and-follow-vertically)

;; Toggle two-window split between horizontal and vertical split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x \\") 'toggle-window-split)

;; Don't open a new window for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Open man pages in other window
(setq Man-notify-method 'aggressive)

;; Auto focus help-mode windows
(setq help-window-select t)

;; Popup windows
(use-package popper
  :defer nil
  :bind
  ("M-`"     . popper-cycle)
  ("C-c p d" . popper-kill-latest-popup)
  ("C-c p t" . popper-toggle-type)
  ;; US kb layout
  ("C-`"     . popper-toggle)
  ;; SV kb layout
  ("C-§"     . popper-toggle)
  :init
  (setq popper-reference-buffers
        '("\\*Compile-Log\\*"
          "^\\*compilation.*\\*$"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*tex-shell.*\\*$"
          "^\\*Flycheck.*\\*$"
          "^\\*LSP Error List*\\*$"
          "^\\*vterminal - dedicated\\*"
          magit-mode
          comint-mode
          shell-mode
          term-mode
          help-mode
          helpful-mode
          compilation-mode
          mpdel-song-mode))
  :config
  (setq popper-mode-line " POP " ; Let it breathe a bit
        popper-window-height 19)
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package hydra
  :bind ("C-c q" . hydra-window/body)
  :config
  ;; Resize window
  (defhydra hydra-window (:timeout 4)
    "
  Resize window:
  -------------
  [_h_] Decrease width
  [_j_] Increase height
  [_k_] Decrease height
  [_l_] Increase width
  [_b_] Balance window sizes
"
    ("h" (my/window-width-decrease)  nil)
    ("j" (my/window-height-increase) nil)
    ("k" (my/window-height-decrease) nil)
    ("l" (my/window-width-increase)  nil)
    ("b" (balance-windows)  nil)
    ("q" nil nil :exit t)))

;; Resizes the window width based on the input
(defun my/resize-window-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window width in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!")))
               (message "%s" w)
               (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t)))

;; Resizes the window height based on the input
(defun my/resize-window-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!")))
               (message "%s" h)
               (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil)))

(defun my/resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

;; Shorcuts for window resize width and height
(defun my/window-width-increase ()
  (interactive)
  (my/resize-window t 5))

(defun my/window-width-decrease ()
  (interactive)
  (my/resize-window t -5))

(defun my/window-height-increase ()
  (interactive)
  (my/resize-window nil 5))

(defun my/window-height-decrease ()
  (interactive)
  (my/resize-window nil -5))

(provide 'cfg-win)
