;; -*- lexical-binding: t; -*-

;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 160))

;; Display right and left fringe
(fringe-mode '(8 . 8))

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Show column number in status bar
(column-number-mode)

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
                elfeed-show-mode-hook))
  (add-hook hook 'hl-line-mode))

;; Make keybindings in minibuffer look like other text
(set-face-attribute 'help-key-binding nil
                    :box nil
                    :foreground 'unspecified'
                    :background 'unspecified'
                    :inherit nil)

;; Set font
(let ((mono-spaced-font "Noto Sans Mono")
      (proportionately-spaced-font "Libertinus Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 110)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.1)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.2))

;; Set background transparency
(add-to-list 'default-frame-alist '(alpha-background . 100))

(use-package modus-themes
  :defer t)

(use-package ef-themes
  :defer t)

(use-package gruvbox-theme
  :defer t)

(use-package nordic-night-theme
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
          eshell-mode
          shell-mode
          term-mode
          vterm-mode
          ansi-term-mode
          help-mode
          helpful-mode
          compilation-mode))
  :config
  (setq popper-mode-line " POP " ; Let it breathe a bit
        popper-window-height 15)
  ;; popper-group-function #'popper-group-by-directory)
  (popper-mode 1)
  (popper-echo-mode 1))

;; Automatically switch focus to new window when it is created
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Temporary command buffers
(use-package hydra
  :config

  (defhydra hydra-main (:timeout 4)
    "
  Choose function:
  ----------------
  [_t_] Choose Theme
  [_r_] Resize Window
"
    ("t" hydra-theme/body nil :exit t)
    ("r" hydra-window/body nil :exit t)
    ("q" nil nil :exit t)))

;; Choose theme and remember it between sessions
(defhydra hydra-theme (:timeout 4)
  "
  Choose theme:
  -------------
  [_o_] Modus Operandi Tinted
  [_l_] EF Melissa Light
  [_g_] Gruvbox
  [_m_] Miasma
  [_n_] Nordic night
  [_v_] Modus Vivendi
"
  ("o" (my/enable-theme 'modus-operandi-tinted) nil)
  ("l" (my/enable-theme 'ef-melissa-light) nil)
  ("g" (my/enable-theme 'gruvbox-dark-soft) nil)
  ("m" (my/enable-theme 'miasma) nil)
  ("n" (my/enable-theme 'nordic-night) nil)
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
  Resize window:
  -------------
  [_h_] Decrease width
  [_j_] Increase height
  [_k_] Decrease height
  [_l_] Increase width
"
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

(provide 'cfg-ui)
