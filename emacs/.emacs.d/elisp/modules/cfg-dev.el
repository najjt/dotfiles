;; -*- lexical-binding: t; -*-

;; Better commenting
(use-package evil-nerd-commenter
  :defer t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Git interface
(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-c g" . magit-status))

;; Highlight vc diff in buffers and dired
(use-package diff-hl
  :defer nil
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

;; Common file types
(use-package yaml-mode)
(use-package json-mode)
(use-package csv-mode)
(use-package markdown-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package ini-mode)

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . (lambda ()
                       (subword-mode)         ; Toggle subword movement
                       (show-paren-mode)      ; Highlight matching parentheses
                       (electric-pair-mode))) ; Insert matching delimiters
  :mode ("\\.rasi\\'"
         "\\.edn\\'"))

(provide 'cfg-dev)
