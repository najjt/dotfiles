;; -*- lexical-binding: t; -*-

(use-package general
  :config

  ;; Open Hydra main menu
  (general-define-key
   :keymaps '(normal visual emacs)
   "," 'hydra-main/body)

  ;; Make <escape> quit prompts
  (general-define-key
   "<escape>" 'keyboard-escape-quit)

  ;; Increase/decrease text size
  (general-define-key
   "C-=" #'text-scale-increase
   "C-+" #'text-scale-increase
   "C--" #'text-scale-decrease))

(provide 'ml-keys)

;; vi emulation
(use-package evil
  :diminish
  :demand t
  :bind
  ("C-z" . evil-local-mode) ; Toggle evil mode

  ;; Window navigation
  (:map evil-normal-state-map
        ("C-w h" . evil-window-left)
        ("C-w j" . evil-window-down)
        ("C-w k" . evil-window-up)
        ("C-w l" . evil-window-right))

  :hook (evil-mode . my/evil-hook)

  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)

  :config
  (evil-set-undo-system 'undo-redo)

  ;; Modes to disable evil in
  (defun my/evil-hook ()
    (dolist (mode '(custom-mode
                    eshell-mode
                    git-rebase-mode
                    erc-mode
                    term-mode
                    vterm-mode
                    ansi-term-mode))
      (add-to-list 'evil-emacs-state-modes mode)))

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

  :custom
  ;; Horizontal movement crosses lines
  (evil-cross-lines t))

;; More vim keybindings (in non-file buffers)
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
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
  :hook (org-mode . evil-org-mode)
  :diminish
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'm-keys)
