;; -*- lexical-binding: t; -*-

;; Show completions in a vertical UI
(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-cycle t))

;; Better completion style
(use-package orderless
  :config
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)   ; Disable defaults, use orderless settings
  (completion-pcm-leading-wildcard t)) ; Emacs 31: partial-completion behaves like substring

;; Completions
(use-package corfu
  :init (global-corfu-mode)
  :config
  ;; Use <TAB> for both indentation & completion
  (setq tab-always-indent 'complete
        completion-cycle-threshold 1))

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Completion extensions
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Annotations for the minibuffer
(use-package marginalia
  :config
  (marginalia-mode 1))

;; More detailed help buffers
(use-package helpful
  :bind (:map helpful-mode-map
              ("q" . (lambda () (interactive) (quit-window))))
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

(use-package embark
  :config
  ;; Use Embark to help with command discovery
  (setq prefix-help-command #'embark-prefix-help-command))

(provide 'cfg-help)
