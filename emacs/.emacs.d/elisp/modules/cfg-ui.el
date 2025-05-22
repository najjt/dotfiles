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

;; Display line numbers in the below modes
(dolist (hook '(conf-mode-hook
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
                Info-mode-hook
                messages-buffer-mode-hook
                elfeed-show-mode-hook))
  (add-hook hook 'hl-line-mode))

;; Make keybindings in minibuffer look like other text
(set-face-attribute 'help-key-binding nil
                    :box nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit nil)

;; Set font
(let ((mono-spaced-font "NotoSansM Nerd Font Mono")
      (proportionately-spaced-font "Libertinus Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 110)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.1)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.2))

(use-package modus-themes
  :defer t)

(defun my/disable-all-themes ()
  "Disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/enable-theme (theme)
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
          "^\\*vterminal - dedicated\\*$"
          magit-mode
          comint-mode
          shell-mode
          term-mode
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
  [_e_] Balance window sizes
"
    ("h" (my/window-width-decrease)  nil)
    ("j" (my/window-height-increase) nil)
    ("k" (my/window-height-decrease) nil)
    ("l" (my/window-width-increase)  nil)
    ("e" (balance-windows)  nil)
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

;; Hide minor modes in modeline
(use-package diminish
  :diminish (auto-fill-function
             centered-window-mode
             eldoc-mode
             evil-collection-unimpaired-mode
             org-indent-mode
             abbrev-mode
             subword-mode
             flymake-mode))

;; Enable background transparency
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Toggle window transparency
;; Source: https://kristofferbalintona.me/posts/202206071000/
(defun my/toggle-window-transparency (arg)
  "Toggle the value of `alpha-background`.

Toggles between 100 and 90 by default. Can choose which value to change
to if called with ARG, or any prefix argument."
  (interactive "P")
  (let ((transparency (pcase arg
                        ((pred numberp) arg)
                        ((pred consp) (read-number "Change the transparency to which value (0-100)? "))
                        (_
                         (pcase (frame-parameter nil 'alpha-background)
                           (90 100)
                           (100 90)
                           (_ 100))))))
    (set-frame-parameter nil 'alpha-background transparency)))

;; Center buffers
(use-package olivetti
  :defer t
  :commands (olivetti-mode)
  :custom (olivetti-body-width 82)
  :init
  (defun my/olivetti-conditional-enable ()
    "Enable `olivetti-mode` only in Org or Markdown buffers."
    (when (derived-mode-p 'org-mode 'markdown-mode)
      (olivetti-mode 1)))

  (defun my/olivetti-conditional-disable ()
    "Disable `olivetti-mode` if not in Org or Markdown buffers."
    (unless (derived-mode-p 'org-mode 'markdown-mode)
      (when (bound-and-true-p olivetti-mode)
        (olivetti-mode -1))))

  ;; Add hooks globally at init time
  (add-hook 'after-change-major-mode-hook #'my/olivetti-conditional-disable)
  (add-hook 'org-mode-hook #'my/olivetti-conditional-enable)
  (add-hook 'markdown-mode-hook #'my/olivetti-conditional-enable))

;; Show full path to file in header line
(defun my/set-header-line-for-files-only ()
  "Set `header-line-format` to show bold file name info only in file-visiting buffers."
  (setq header-line-format
        (when buffer-file-name
          (let ((file-name (abbreviate-file-name buffer-file-name)))
            (concat (propertize file-name 'face 'bold))))))

(add-hook 'after-change-major-mode-hook #'my/set-header-line-for-files-only)

(provide 'cfg-ui)
