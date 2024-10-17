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

;; Auto reload non-file buffers
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

(use-package embark-consult
  :after embark
  :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

;; Dotfiles management
(use-package chezmoi
  :bind (("C-c C s" . chezmoi-write)
         ("C-c C f" . chezmoi-find)))

(provide 'ml-general)
