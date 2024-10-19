(use-package vterm
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

;; Open multiple vterm buffers
(use-package multi-vterm
  :bind
  ("C-c t" . multi-vterm-dedicated-toggle)
  ("C-c C-t" . multi-vterm)
  :config
  ;; Dedicated terminal height
  (setq multi-vterm-dedicated-window-height-percent 30)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state))))

(provide 'm-term)
