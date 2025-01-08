;; -*- lexical-binding: t; -*-

;; User information
(setq user-full-name "Martin Lönn Andersson")
(setq user-mail-address "mlonna@pm.me")

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
      browse-url-generic-program "qutebrowser")

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

(provide 'm-general)
