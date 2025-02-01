;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; Only show diagnostics for the current file
  (setq lsp-modeline-diagnostics-scope :file
        ;; Hide icons in the breadcrumb
        lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-enable-which-key-integration t))

;; Tree view for different aspects of your code
(use-package lsp-treemacs
  :after lsp)

;; UI enhancements for lsp mode
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Java support for lsp mode
(use-package lsp-java
  :hook (java-mode . lsp-deferred))

;; Completions
(use-package company
  :after lsp-mode
  :hook ((lsp-mode). company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; UI enhancements for company
(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

;; Better commenting functionality
(use-package evil-nerd-commenter
  :defer t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Match delimiters
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . (lambda ()
                       (highlight-parentheses-mode)
                       (electric-pair-mode))))

;; Git interface
(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-c g" . magit-status))

;; Common file types
(use-package yaml-mode)

(use-package json-mode)

(use-package csv-mode)

(use-package markdown-mode)

(use-package lua-mode)

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . subword-mode)
  :mode ("\\.rasi\\'"
         "\\.edn\\'"))

(provide 'm-dev)
