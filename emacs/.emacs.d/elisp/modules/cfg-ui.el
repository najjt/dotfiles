;; -*- lexical-binding: t; -*-

;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Show column number in status bar
(column-number-mode)

;; Show fringes
(fringe-mode 8)

;; Hide tab bar if only one tab
(setq tab-bar-show 1)

;; Make line numbers relative
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; Display relative line numbers in the below modes
(dolist (hook '(fundamental-mode
                conf-mode-hook
                prog-mode-hook
                text-mode-hook
                markdown-mode-hook
                org-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;; Highlight current line
(dolist (hook '(text-mode-hook
                prog-mode-hook
                dired-mode-hook
                Man-mode-hook
                conf-mode-hook
                emms-browser-mode-hook
                emms-playlist-mode-hook
                org-agenda-mode-hook
                elfeed-show-mode-hook))
  (add-hook hook 'hl-line-mode))

;; Make keybindings in minibuffer look like other text
(set-face-attribute 'help-key-binding nil
                    :box nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit nil)

;; Set font
(let ((mono-spaced-font "Noto Sans Mono")
      (proportionately-spaced-font "Libertinus Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 110)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.1)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.2))

(use-package modus-themes
  :defer t)

(use-package gruvbox-theme
  :defer t)

(use-package miasma-theme
  :defer t)

;; Popup buffers
(use-package popper
  :bind
  ("C-`"     . popper-toggle)
  ("M-`"     . popper-cycle)
  ("C-M-`"   . popper-toggle-type)
  ("C-c p d" . popper-kill-latest-popup)
  :init
  (setq popper-reference-buffers
        '("\\*Compile-Log\\*"
          "^\\*compilation.*\\*$"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*tex-shell.*\\*$"
          "^\\*Flycheck.*\\*$"
          "^\\*LSP Error List*\\*$"
          magit-mode
          comint-mode
          shell-mode
          term-mode
          vterm-mode
          help-mode
          helpful-mode
          compilation-mode))
  :config
  (setq popper-mode-line " POP " ; Let it breathe a bit
        popper-window-height 15)
  (popper-mode 1)
  (popper-echo-mode 1))

;; Temporary command buffers
(use-package hydra
  :config

  (defhydra hydra-main (:timeout 4)
    "
  Resize window:
  -------------
  [_h_] Decrease width
  [_j_] Increase height
  [_k_] Decrease height
  [_l_] Increase width
"
  ("h" (my/window-width-decrease)  nil)
  ("j" (my/window-height-increase) nil)
  ("k" (my/window-height-decrease) nil)
  ("l" (my/window-width-increase)  nil)
  ("q" nil nil :exit t))

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

(provide 'cfg-ui)
