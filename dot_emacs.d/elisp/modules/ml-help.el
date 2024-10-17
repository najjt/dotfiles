;; Show completions in a vertical UI
(use-package vertico
  :init
  (vertico-mode))

;; Search and navigation commands
(use-package consult
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line))

;; Better completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless)))

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
  :diminish
  :config (which-key-mode 1))

(provide 'ml-help)
