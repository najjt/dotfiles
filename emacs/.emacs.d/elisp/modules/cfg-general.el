;; -*- lexical-binding: t; -*-

;; User information
(setq user-full-name "Martin Lönn Andersson")
(setq user-mail-address "martin@malon.se")

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

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
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Automatically reload non-file buffers
(setq global-auto-revert-non-file-buffers t)

;; Edit files with sudo privileges
(use-package sudo-edit
  :defer t
  :diminish
  :bind (("C-c C-r" . sudo-edit)
         ("C-c C-t" . sudo-edit-find-file)))

;; Provides commands to run based on current context
(use-package embark
  :defer nil
  :bind ("M-;" . embark-act))

;; Integrate embark with consult
(use-package embark-consult
  :after embark
  :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

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

;; Don't open a new window for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Open man pages in current window
(setq Man-notify-method 'pushy)

;; Use a single buffer for the dictionary
(setq dictionary-use-single-buffer t)

;; Use local dictionary server
;; See https://www.masteringemacs.org/article/wordsmithing-in-emacs
(setq dictionary-server "localhost")

;; Copy to system clipboard in terminal
(unless (display-graphic-p)
  (use-package xclip
    :config
    (xclip-mode)))

(use-package isearch
  :ensure nil
  :hook (occur-mode . turn-off-evil-mode)
  :config

  ;; Open occurrences of current isearch in new buffer
  ;; Source: https://blog.chmouel.com/posts/emacs-isearch/
  (defun my/occur-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (occur query)))

  ;; Use selection to search
  ;; Source: https://blog.chmouel.com/posts/emacs-isearch/
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
        ("C-d" . isearch-forward-symbol-at-point)))

(defun my/select-window (window &rest _)
  "Select WINDOW for display-buffer-alist"
  (select-window window))

(setq display-buffer-alist
      '(((or . ((derived-mode . occur-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (body-function . my/select-window)
         (dedicated . t)
         (preserve-size . (t . t)))))

;; Use minibuffer for epa password interface
(setq egp-pinentry-mode 'loopback)

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

(global-set-key (kbd "C-x |") 'toggle-window-split)

(use-package pdf-tools
  :defer t
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
        '(;; Prot's commentary on life
          "https://protesilaos.com/commentary.xml"
          ;; Prot's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g"
          ;; Bread on Penguins's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCwHwDuNd9lCdA7chyyquDXw"
          "https://www.debian.org/News/news"
          "https://www.femtejuli.se/feed")))

(use-package vterm
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

;; Open multiple vterm buffers
(use-package multi-vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local evil-insert-state-cursor 'box)
                        (evil-insert-state)))
  :bind
  ("C-c t" . multi-vterm-dedicated-toggle)
  ("C-c T" . multi-vterm)
  :config
  ;; Dedicated terminal height
  (setq multi-vterm-dedicated-window-height-percent 30))


;; Focus new frames
(defun my/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))
(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

;; Use Emacs minibuffer for GPG pinentry
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)

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

(provide 'cfg-general)
