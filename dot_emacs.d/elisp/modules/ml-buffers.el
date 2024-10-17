;; Popup buffers
(use-package popper
  :bind
  ("C-0"   . popper-toggle)
  ("M-p"   . popper-cycle)
  ("C-M-0" . popper-toggle-type)
  ("C-c d" . popper-kill-latest-popup)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "^\\*compilation.*\\*$"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*tex-shell.*\\*$"
          "^\\*Flycheck.*\\*$"
          "^\\*Buffer List*\\*$"
          "^\\*LSP Error List*\\*$"
          magit-mode
          comint-mode
          eshell-mode
          shell-mode
          term-mode
          vterm-mode
          ansi-term-mode
          help-mode
          helpful-mode
          compilation-mode))
  :config
  (setq popper-mode-line " POP " ; Let it breathe a bit
        popper-window-height 15
        popper-group-function #'popper-group-by-directory)
  (popper-mode 1)
  (popper-echo-mode 1))

;; Automatically switch focus to new window when it is created
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(provide 'ml-buffers)
