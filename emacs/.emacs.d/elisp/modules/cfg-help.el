;; -*- lexical-binding: t; -*-

;; Show completions in a vertical UI
(use-package vertico
  :config
  (vertico-mode))

;; Better completion style
(use-package orderless
  :config
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)   ; Disable defaults, use orderless settings
  (completion-pcm-leading-wildcard t)) ; Emacs 31: partial-completion behaves like substring

;; Completion extensions
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Completions
(use-package corfu
  :init (global-corfu-mode)
  :config
  ;; Use <TAB> for both indentation & completion
  (setq tab-always-indent 'complete))

;; Annotations for the minibuffer
(use-package marginalia
  :config
  (marginalia-mode 1))

;; More detailed help buffers
(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

;; Display help for next command keystroke
(use-package which-key
  :ensure nil
  :diminish
  :config (which-key-mode 1))

(provide 'cfg-help)
