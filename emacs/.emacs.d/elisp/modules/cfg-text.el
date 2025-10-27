;; -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")

;; Automatic line breaking
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)

;; Spell checking
(use-package jinx
  :diminish
  :hook (((markdown-mode org-mode text-mode) . jinx-mode))
  :bind ("C-c s" . jinx-correct)
  :config
  (setq jinx-languages "sv en_US"))

;; Undo functionality
(use-package vundo
  :defer t)

;; Tabs are four spaces
(setq-default tab-width 4 indent-tabs-mode nil)

(provide 'cfg-text)
