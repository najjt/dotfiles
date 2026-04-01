;; -*- lexical-binding: t; -*-

;; Additional keybinding for M-x
(global-set-key (kbd "C-c k") 'execute-extended-command)

;; Increase/decrease text scale
(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "C-+" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)

;; Make escape quit prompts
(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; Keybind workarounds for terminal Emacs
(define-key key-translation-map (kbd "C-x ,") (kbd "C-x C-;")) ; comment-line
(define-key key-translation-map (kbd "C-c ,") (kbd "C-c C-,")) ; org-insert-structure-template

(defun my/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] #'my/smart-open-line)

;; vi emulation
(use-package evil
  :diminish
  :demand t
  :bind
  ("C-z" . evil-local-mode) ; Toggle evil mode
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-i-jump nil)

  :config
  (evil-set-undo-system 'undo-redo)

  ;; Turn on evil mode
  (evil-mode 1)

  ;; Move on visual lines unless a count is involved
  (with-eval-after-load 'evil
    (evil-define-motion evil-next-line (count)
      "Move the cursor COUNT screen lines down."
      :type line
      (let ((line-move-visual (unless count t)))
        (evil-line-move (or count 1))))

    (evil-define-motion evil-previous-line (count)
      "Move the cursor COUNT lines up."
      :type line
      (let ((line-move-visual (unless count t)))
        (evil-line-move (- (or count 1))))))

  ;; Indicate current evil state in terminal environment
  (use-package evil-terminal-cursor-changer
    :config
    (unless (display-graphic-p)
      (evil-terminal-cursor-changer-activate)))

  ;; Undo functionality
  (use-package vundo
    :defer t)
  :custom
  ;; Horizontal movement crosses lines
  (evil-cross-lines t))

;; More vim keybindings (in non-file buffers)
(use-package evil-collection
  :after evil
  :diminish
  :config
  (evil-collection-init)

  ;; vim-style navigation in dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

;; Even more vim keybindings (adds surround functionality)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; vim keybindings for org mode
(use-package evil-org
  :after org
  :hook ((org-mode org-agenda-mode) . evil-org-mode)
  :diminish
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'cfg-keys)
