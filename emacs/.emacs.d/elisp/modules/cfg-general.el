;; -*- lexical-binding: t; -*-

;; User information
(setq user-full-name "Martin Lönn Andersson")
(setq user-mail-address "martin@malon.se")

;; Get environment variables from your shell
(use-package exec-path-from-shell
  :config
  ;; Don't start an interactive shell (improves startup time)
  (setq exec-path-from-shell-arguments nil)

  ;; Which environment variables to import
  (dolist (var '("LANG" "LC_ALL" "PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Save text entered in minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

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
  :config
  (global-set-key (kbd "C-c C-r") 'sudo-edit))

;; Provides commands to run based on current context
(use-package embark
  :bind ("M-;" . embark-act))

;; Integrate embark with consult
(use-package embark-consult
  :after embark
  :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

;; More markdown support
(use-package markdown-mode
  :defer t)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "librewolf")

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
  :defer t
  :hook (occur-mode . turn-off-evil-mode)
  :config

  ;; Open occurrences of current isearch in new buffer
  ;; Source: https://blog.chmouel.com/posts/emacs-isearch/
  (defun my-occur-from-isearch ()
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
        ("C-o" . my-occur-from-isearch)
        ("C-d" . isearch-forward-symbol-at-point)))

(defun my-select-window (window &rest _)
  "Select WINDOW for display-buffer-alist"
  (select-window window))

(setq display-buffer-alist
      '(((or . ((derived-mode . occur-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (body-function . my-select-window)
         (dedicated . t)
         (preserve-size . (t . t)))))

;; Use minibuffer for epa password interface
(setq egp-pinentry-mode 'loopback)

(provide 'cfg-general)
