;; -*- lexical-binding: t; -*-

;; User information
(setq user-full-name "Martin Lönn Andersson"
      user-mail-address "martin@malon.se")

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser"
      browse-url-generic-args '("--target" "window"))

;; Get environment variables from your shell
(use-package exec-path-from-shell
  :config
  ;; Don't start an interactive shell (improves startup time)
  (setq exec-path-from-shell-arguments nil)

  ;; Which environment variables to import
  (dolist (var '("LANG" "LC_ALL" "PATH" "ZDOTDIR"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Save text entered in minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Save place in files
(save-place-mode 1)

;; Remember recently edited files
(recentf-mode 1)

;; Automatically reread files when changed
(setopt auto-revert-avoid-polling t
        auto-revert-interval 5
        auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Automatically reload non-file buffers
(setq global-auto-revert-non-file-buffers t)

;; Edit files with sudo privileges
(use-package sudo-edit
  :defer t
  :diminish)

;; Increase large file warning threshold
(setq large-file-warning-threshold 100000000)

;; Clean up unneccesary whitespace on save,
;; unless in markdown mode
(defun my/whitespace-cleanup ()
  (unless (derived-mode-p 'markdown-mode)
    (whitespace-cleanup)))
(add-hook 'before-save-hook 'my/whitespace-cleanup)

;; Map yes and no to y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable visual and audible bell
(setq ring-bell-function 'ignore)

;; Suppress auto revert messages
(setq auto-revert-verbose nil)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; Add a newline automatically at the end of the file upon save
(setq require-final-newline t)

;; Make switching buffers more consistent
(setopt switch-to-buffer-obey-display-actions t)

;; Smooth scrolling
(setq scroll-step 10
      scroll-margin 10
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      hscroll-step 3
      hscroll-margin 3)

;; Dictionary
(setq dictionary-use-single-buffer t ; Use a single buffer for the dictionary
      dictionary-server "localhost") ; Use local dictionary server

;; Copy to system clipboard in terminal
(unless (display-graphic-p)
  (use-package xclip
    :config
    (xclip-mode)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("M-n" . pdf-view-next-page)
              ("M-p" . pdf-view-previous-page))
  :config
  (pdf-tools-install))

;; RSS feeds
(use-package elfeed
  :bind ("C-c w" . (lambda ()
                     (interactive)
                     (elfeed)
                     (elfeed-update)))
  :config
  (setq elfeed-feeds
        '(;; Bread on Penguins's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCwHwDuNd9lCdA7chyyquDXw"
          "https://www.privacyguides.org/articles/feed_rss_created.xml")))

(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local evil-insert-state-cursor 'box)
                        (evil-insert-state)))
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

;; Open multiple vterm buffers
(use-package multi-vterm
  :bind
  ("C-c t" . multi-vterm-dedicated-toggle)
  ("C-c C-x t" . multi-vterm)
  :config
  ;; Dedicated terminal height
  (setq multi-vterm-dedicated-window-height-percent 30))

;; Focus new frames
(defun my/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))
(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

;; Use Emacs minibuffer for GPG pinentry
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)

;; Refresh packages when using package-install
;; if last refresh was longer than 24 hours ago
;; Source: https://andreyor.st/posts/2022-07-15-refresh-package-contents-automatically/
(defcustom package-last-refresh-date nil
  "Date and time when package lists have been refreshed.

  This variable is then used to check whether
  `package-refresh-contents' call is needed before calling
  `package-install'. The value of this variable is updated when
  `package-refresh-contents' is called.

  See `package-refresh-hour-threshold' for the amount of time needed to
  trigger a refresh."
  :type 'string
  :group 'package)

(defcustom package-automatic-refresh-threshold 24
  "Amount of hours since last `package-refresh-contents' call
  needed to trigger automatic refresh before calling `package-install'."
  :type 'number
  :group 'package)

(define-advice package-install (:before (&rest _) package-refresh-contents-maybe)
  (when (or (null package-last-refresh-date)
            (> (/ (float-time
                   (time-subtract (date-to-time (format-time-string "%Y-%m-%dT%H:%M"))
                                  (date-to-time package-last-refresh-date)))
                  3600)
               package-automatic-refresh-threshold))
    (package-refresh-contents)))

(define-advice package-refresh-contents (:after (&rest _) update-package-refresh-date)
  (customize-save-variable 'package-last-refresh-date
                           (format-time-string "%Y-%m-%dT%H:%M")))

;; Set default alert style to send
;; desktop notifications
(setq alert-default-style 'libnotify)

(use-package isearch
  :ensure nil
  :defer t
  :config
  ;; Open occur from current isearch results
  (defun my/occur-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
               isearch-string
             (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (occur query)))

  ;; use selection to search
  (defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
    (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
        (progn
          (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
          (deactivate-mark)
          ad-do-it
          (if (not forward)
              (isearch-repeat-backward)
            (goto-char (mark))
            (isearch-repeat-forward)))
      ad-do-it))
  :bind
  (:map isearch-mode-map
        ("C-o" . my/occur-from-isearch)
        ("C-d" . isearch-forward-symbol-at-point)
        ("C-h" . isearch-query-replace)))

(when (eq system-type 'gnu/linux)
  (use-package sxhkdrc-mode))

;; Search and navigation commands
(use-package consult
  :defer nil
  :bind ("C-c f" . consult-find)
  :config
  (setq-default consult-find-args "find .")

  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line))

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

(provide 'cfg-general)
