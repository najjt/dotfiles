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
(use-package vundo)

;; Tabs are four spaces
(setq-default tab-width 4 indent-tabs-mode nil)

;; Visualize color codes in text
(use-package colorful-mode
  :diminish
  :custom
  (colorful-use-prefix nil)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package go-translate
  :bind ("C-c o" . gt-do-translate)
  :config
  (setq gt-langs '(en sv))
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :text 'word :prompt t)
         :engines (gt-google-engine)
         :render  (gt-buffer-render))))

(provide 'cfg-text)
