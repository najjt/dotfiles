(setq package-enable-at-startup nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar better-gc-cons-threshold 4294967269; 512mb
  "The default value to use for `gc-cons-threshold'.

If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

(add-hook 'emacs-startup-hook
      (lambda ()
        (if (boundp 'after-focus-change-function)
        (add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))
      (add-hook 'after-focus-change-function 'garbage-collect))
        (defun gc-minibuffer-setup-hook ()
      (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

        (defun gc-minibuffer-exit-hook ()
      (garbage-collect)
      (setq gc-cons-threshold better-gc-cons-threshold))

        (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
        (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(when *sys/linux*
  (setq x-super-keysym 'alt))

(when *sys/mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq dired-use-ls-dired nil)
  (setq frame-resize-pixelwise t)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
  (add-to-list 'load-path base))
    (dolist (f (directory-files base))
  (let ((name (concat base "/" f)))
    (when (and (file-directory-p name)
           (not (equal f ".."))
           (not (equal f ".")))
      (unless (member base load-path)
        (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))

(load-file "~/.emacs.d/custom.el")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("elpa" . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("gnu-devel" . "https://elpa.gnu.org/devel/")
    ("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")))

(package-initialize)

;; ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq
 use-package-always-ensure t
 use-package-verbose t)

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package gnu-elpa-keyring-update)

(when *sys/linux*
  (use-package exwm
    :config
    ;; show time
    (setq display-time-default-load-average nil)
    (display-time-mode t)

    (ido-mode 1)

    ;; load exwm
    (require 'exwm)

    ;; fix problems with Ido
    (require 'exwm-config)
    (exwm-config-ido)

    (add-hook 'exwm-update-class-hook
              (lambda ()
                (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-class-name))))
    (add-hook 'exwm-update-title-hook
              (lambda ()
                (when (or (not exwm-instance-name)
                          (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-title))))

    (setq exwm-input-global-keys
          `(
            ;; Bind "s-r" to exit char-mode and fullscreen mode.
            ([?\s-r] . exwm-reset)
            ;; Bind "s-w" to switch workspace interactively.
            ([?\s-w] . exwm-workspace-switch)
            ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))
            ;; Bind "s-&" to launch applications ('M-&' also works if the output
            ;; buffer does not bother you).
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; Bind "s-<f2>" to "slock", a simple X display locker.
            ([s-f2] . (lambda ()
                        (interactive)
                        (start-process "" nil "/usr/bin/slock")))))

    ;; To add a key binding only available in line-mode, simply define it in
    ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

    ;; The following example demonstrates how to use simulation keys to mimic
    ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
    ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
    ;; and DEST is what EXWM actually sends to application.  Note that both SRC
    ;; and DEST should be key sequences (vector or string).
    (setq exwm-input-simulation-keys
          '(
            ;; movement
            ([?\C-b] . [left])
            ([?\M-b] . [C-left])
            ([?\C-f] . [right])
            ([?\M-f] . [C-right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete])
            ;; cut/paste.
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ;; search
            ([?\C-s] . [?\C-f])))

    ;; You can hide the minibuffer and echo area when they're not used, by
    ;; uncommenting the following line.
                                        ;(setq exwm-workspace-minibuffer-position 'bottom)

    ;; Do not forget to enable EXWM. It will start by itself when things are
    ;; ready.  You can put it _anywhere_ in your configuration.
    (exwm-enable)))

(setq user-full-name "Martin Lönn Andersson")
(setq user-mail-address "mlonna@pm.me")

(use-package chezmoi)

(use-package exec-path-from-shell
  :config
  ;; which environment variables to import
  (dolist (var '("LANG" "LC_ALL"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package swiper :diminish)

(use-package markdown-mode :defer t)

(use-package flyspell
  :diminish flyspell-mode
  :hook
  ((markdown-mode org-mode text-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :bind
  ("C-l" . flyspell-auto-correct-previous-word)
  :config
  (with-eval-after-load "ispell"
    (setenv "LANG" "en_US.UTF-8")
    (setenv "LC_ALL" "en_US.UTF-8")
    (setq ispell-program-name "hunspell"
          ispell-local-dictionary "sv_SE"
          ispell-local-dictionary-alist
          '(("sv_SE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "sv_SE,en_US") nil utf-8))
          ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
          ispell-personal-dictionary "~/resources/spelling/.hunspell_personal")))

;; save text entered in minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; save cursor position in files
(save-place-mode 1)

;; remember recently edited files
(recentf-mode 1)

;; auto reload non-file buffers
(setq global-auto-revert-non-file-buffers t)

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t))

(use-package general
  :config
  ;; leader key for hydras
  (general-create-definer my/leader-keys
    :keymaps '(normal visual emacs)
    :prefix ","
    :global-prefix ",")

  ;; make esc quit prompts
  (general-define-key
   "<escape>" 'keyboard-escape-quit)

  (general-define-key
   "C-=" #'text-scale-increase
   "C-+" #'text-scale-increase
   "C--" #'text-scale-decrease))

(use-package evil
  :diminish
  :demand t
  :bind
  ("C-z" . evil-local-mode)

  (:map evil-normal-state-map
        ("C-w h" . evil-window-left)
        ("C-w j" . evil-window-down)
        ("C-w k" . evil-window-up)
        ("C-w l" . evil-window-right))

  :hook
  (evil-mode . my/evil-hook)

  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-search-module 'evil-search)

  :config
  (defun my/evil-hook () ; modes to disable evil in
    (dolist (mode '(custom-mode
                    eshell-mode
                    git-rebase-mode
                    erc-mode
                    term-mode
                    ansi-term-mode))
      (add-to-list 'evil-emacs-state-modes mode)))

  (evil-mode +1)

  ;; move on visual lines unless a count is involved
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
  (evil-undo-system 'undo-tree)

  ;; horizontal movement crosses lines
  (evil-cross-lines t))

;; more vim keybindings (in non-file buffers)
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; even even more vim keybindings (adds surround functionality)
(use-package evil-surround
  :config
  (global-evil-surround-mode +1))

(use-package hydra
  :config
  (my/leader-keys
    "t" '(hydra-theme/body :which-key "choose theme")
    "r" '(hydra-window/body :which-key "resize window")
    "s" '(hydra-text-scale/body :which-key "scale text")
    "a" '(org-agenda :which-key "open org agenda")
    "c" '(my/custom-open-calendar :which-key "open calendar")))

(defhydra hydra-theme (:timeout 4)
  "choose theme"
  ("l" (my/enable-theme 'standard-light) "standard-light")
  ("v" (my/enable-theme 'modus-vivendi) "modus-vivendi")
  ("t" (my/enable-theme 'doom-tokyo-night) "tokyo night")
  ("f" nil "finished" :exit t))

(defun my/disable-all-themes ()
  "Disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/enable-theme (theme)
  "Enable the specified THEME and disable all other themes."
  (my/disable-all-themes)
  (load-theme theme t)
  (customize-save-variable 'my-chosen-theme theme))

(add-hook 'after-init-hook
          (lambda ()
            (if (boundp 'my-chosen-theme)
                (my/enable-theme my-chosen-theme)
              (my/enable-theme 'modus-vivendi))))

(defhydra hydra-window (:timeout 4)
  "resize window"
  ("h" (window-width-decrease) "decrease width")
  ("j" (window-height-increase) "increase height")
  ("k" (window-height-decrease) "decrease height")
  ("l" (window-width-increase) "increase width")
  ("f" nil "finished" :exit t))

;; resizes the window width based on the input
(defun resize-window-width (w)
  "Resizes the window width based on W."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window width in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" w)
  (window-resize nil (- (truncate (* (/ w 10.0) (frame-width))) (window-total-width)) t))

;; resizes the window height based on the input
(defun resize-window-height (h)
  "Resizes the window height based on H."
  (interactive (list (if (> (count-windows) 1)
                         (read-number "Set the current window height in [1~9]x10%: ")
                       (error "You need more than 1 window to execute this function!"))))
  (message "%s" h)
  (window-resize nil (- (truncate (* (/ h 10.0) (frame-height))) (window-total-height)) nil))

(defun resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

;; shorcuts for window resize width and height
(defun window-width-increase ()
  (interactive)
  (resize-window t 5))

(defun window-width-decrease ()
  (interactive)
  (resize-window t -5))

(defun window-height-increase ()
  (interactive)
  (resize-window nil 5))

(defun window-height-decrease ()
  (interactive)
  (resize-window nil -5))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . (lambda () (dired-hide-details-mode)))
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-free-space nil)
  :config
  (when *sys/mac*
    (setq insert-directory-program "gls"))
  (setq dired-listing-switches "-lAX --group-directories-first")
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  (use-package nerd-icons-dired ; use nerd icons in dired
    :diminish
    :hook
    (dired-mode . nerd-icons-dired-mode)))

;; helpful ui additions
(use-package counsel
  :diminish
  :bind
  ("M-x" . counsel-M-x)
  ("C-S-j" . counsel-switch-buffer)
  ("C-x C-f" . counsel-find-file)

  :config
  (counsel-mode +1))

(use-package ivy
  :diminish
  :bind
  ("C-s" . swiper)
  (:map ivy-switch-buffer-map
        ("C-d" . ivy-switch-buffer-kill))
  (:map ivy-reverse-i-search-map
        ("C-d" . ivy-reverse-i-search-kill))

  :config
  (ivy-mode 1)
  ;; hide "^" from ivy minibuffer
  (setq ivy-initial-inputs-alist nil))

;; helpful information for functions in minibuffers
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; command history for ivy
(use-package prescient)

;; ivy integration for prescient
(use-package ivy-prescient
  :init
  (ivy-prescient-mode 1))

;; more detailed help pages
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; display help for next command keystroke
(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

(use-package vterm
  :commands vterm
  :bind ("C-x t" . vterm)
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      hscroll-step 1
      hscroll-margin 1)

(use-package perspective
  :hook (persp-created . dashboard-open)
  :bind
  ("C-M-j" . persp-counsel-switch-buffer)
  ("C-M-b" . persp-switch)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package persp-projectile
  :after perspective)

;; display right and left fringe
(fringe-mode '(8 . 8))

;; turn off blinking cursor
(blink-cursor-mode 0)

;; show column number in status bar
(column-number-mode)

;; soft-wrap text
(global-visual-line-mode t)

;; tabs are four spaces
(setq-default tab-width 4
              indent-tabs-mode nil)

(when *sys/linux*
  (add-to-list 'default-frame-alist '(font . "Jetbrains Mono-13")))

(when *sys/mac*
  (add-to-list 'default-frame-alist '(font . "Jetbrains Mono-15")))

(use-package nerd-icons)

(use-package standard-themes)

(use-package doom-themes)

(use-package modus-themes)

;; disable border around modelines
(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))

(use-package popper
  :bind
  ("C-å"   . popper-toggle)
  ("M-å"   . popper-cycle)
  ("C-M-å" . popper-toggle-type)
  ("C-x d" . popper-kill-latest-popup)
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
      "\\*Warnings\\*"
      "\\*Compile-Log\\*"
      "^\\*compilation.*\\*$" comint-mode
      "Output\\*$"
      help-mode
      helpful-mode
      compilation-mode
      "\\*Async Shell Command\\*"
      "^\\*eshell.*\\*$" eshell-mode
      "^\\*shell.*\\*$"  shell-mode
      "^\\*term.*\\*$"   term-mode
      "^\\*vterm.*\\*$"  vterm-mode
      "^\\*ansi-term.*\\*$"  ansi-term-mode
      "^\\*tex-shell.*\\*$"
      "^\\*Flycheck.*\\*$"
      "^\\*Buffer List*\\*$"
      "^\\*LSP Error List*\\*$"
      "^\\*Dart Run out*\\*$"
      "^magit:.*$"
      "^\\*.*Agenda.*\\*$"))
  :config
  (setq popper-group-function #'popper-group-by-perspective
        popper-mode-line " POP "
        popper-window-height 20)
  (popper-mode 1)
  (popper-echo-mode 1))

(dolist (hook '(fundamental-mode conf-mode-hook prog-mode-hook text-mode-hook markdown-mode-hook org-mode-hook))
  (add-hook hook 'display-line-numbers-mode)
  )

(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t
      )

(defun my/custom-open-org-agenda ()
  (interactive)
  (org-agenda nil "s"))


(use-package dashboard
  :demand t
  :diminish (dashboard-mode page-break-lines-mode)
  :hook (dashboard-after-initialize . my/custom-open-org-agenda)
  :custom-face
  (dashboard-items-face ((t (:weight normal))))
  :custom
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-init-info
                               dashboard-insert-newline
                               dashboard-insert-items))
  (dashboard-items '((bookmarks . 7)
                     (projects . 5)))
  :config
  (dashboard-setup-startup-hook)

  (setq dashboard-center-content t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t
        dashboard-projects-backend 'projectile
        dashboard-projects-switch-function 'projectile-persp-switch-project))

;; hook dashboard-open to creation of new frame
(add-hook 'after-make-frame-functions
        (lambda (frame)
          (with-selected-frame frame
            (dashboard-open))))

(use-package perfect-margin
  :diminish
  :custom
  (perfect-margin-visible-width 100)
  :config
  (perfect-margin-mode t)
  (setq perfect-margin-ignore-modes
        '(dired-mode)))

(use-package prog-mode
  :ensure nil
  :mode ("\\.edn\\'" "\\.lua\\'"))

(use-package dape)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . (lambda () (rainbow-delimiters-mode))))

(use-package projectile
  :diminish
  :custom (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode 1)
  (setq projectile-ignored-projects '("~/.cfg" "~/.emacs.d" "~/Projects/pathfinder")
        projectile-track-known-projects-automatically nil)

  (use-package counsel-projectile
    :config (counsel-projectile-mode 1)))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x m" . magit-status))

(with-eval-after-load 'treemacs
  (treemacs-resize-icons 15))



(use-package tex
  :ensure auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                             TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (reftex-isearch-minor-mode)
                  (setq reftex-plug-into-AUCTeX t
                        TeX-PDF-mode t
                        TeX-source-correlate-method 'synctex
                        TeX-source-correlate-start-server t))))

(setq package-selected-packages
  '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
    ;; Optional packages
    lsp-ui company hover))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'dart-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(use-package org
  :pin nongnu
  :ensure org-contrib ; needed for org-contacts
  :hook (org-mode . org-indent-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq org-directory "~/Documents/notes/org"
        org-default-notes-file (concat org-directory "/capture.org")
        org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))
        org-tags-column 0
        org-startup-folded t
        org-export-backends '(md org ascii html icalendar latex odt rss)
        org-ellipsis " ▾"
        org-blank-before-new-entry (quote ((heading . nil)
                                           (plain-list-item . nil))))

  ;; make only first org heading be bold
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :weight normal))))
   '(org-level-3 ((t (:inherit outline-3 :weight normal))))
   '(org-level-4 ((t (:inherit outline-4 :weight normal))))
   '(org-level-5 ((t (:inherit outline-5 :weight normal))))
   '(org-level-6 ((t (:inherit outline-6 :weight normal))))
   '(org-level-7 ((t (:inherit outline-7 :weight normal))))
   '(org-level-8 ((t (:inherit outline-8 :weight normal)))))

  ;; remap org indentation keys
  (with-eval-after-load 'org
    (general-define-key
     :keymaps 'org-mode-map
     "C-c i" 'org-metaright
     "C-c u" 'org-metaleft)))

;; change default bullets
(use-package org-bullets
  :after org
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-agenda
  :ensure nil
  :after org
  :config
  (setq org-agenda-span 'day
        org-agenda-tags-column 0
        org-agenda-start-on-weekday nil
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-todo-list-sublevels t
        ;; org element cache often produced errors, so I disabled it
        org-element-use-cache nil
        org-agenda-scheduled-leaders '("" "")  ; hide "Scheduled" text
        org-agenda-prefix-format "  %?-12t% s" ; hide category for agenda items

        ;; add newline above date heading
        org-agenda-format-date
        (lambda (date)
          (concat "\n" (org-agenda-format-date-aligned date)))

        ;; time grid settings
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "...." "------------")
        org-agenda-current-time-string
        "← now")

  (setq org-agenda-custom-commands
        '(("s" "Super agenda"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Schedule"
                                  :time-grid t)
                           (:name "Vanor"
                                  :habit t)
                           (:name "Overdue"
                                  :deadline past
                                  :scheduled past)
                           (:name "Studier"
                                  :and (:category "studier" :scheduled today)
                                  :and (:category "studier" :deadline today))
                           (:name "Privat"
                                  :and (:category ("privat" "capture" "computer") :scheduled today)
                                  :and (:category ("privat" "capture" "computer") :deadline today))
                           (:name "Upcoming Deadlines"
                                  :deadline future)
                           (:discard (:anything t))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Priority Items"
                                   :priority>= "C")
                            (:discard (:anything t))))))))))

  ;; date heading settings
  (custom-set-faces
   '(org-agenda-date ((t (:height 1.0 :weight bold :background nil))))
   '(org-agenda-date-today ((t (:height 1.3 :weight bold :background nil :underline nil))))))

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode 1))

(use-package org-capture
  :ensure nil
  :after org
  :config
  ;; don't save org capture bookmarks
  (setq org-bookmark-names-plist nil
        org-capture-bookmark nil)
  :custom
  (org-capture-templates
   '(
     ("t" "Task" entry (file "")
      "* TODO %?\n  %i\n")

     ("l" "Task with link" entry (file "")
      "* TODO %?\n  %i\n %a")

     ("n" "Note" entry (file "")
      "* %?\n %i\n")

     ("e" "Calendar event" entry (file "calendar.org")
      "* %?\n %^t")

     ("b" "Book" entry (file+headline "backlog.org" "Books")
      "* %?\n %i\n")

     ("m" "Movie" entry (file+headline "backlog.org" "Movies")
      "* %?\n %i\n")

     ("w" "Web" entry (file+headline "backlog.org" "Web")
      "* %i\n%U\n\n")

     ("c" "Contact" entry (file "")
      "* %?
        :PROPERTIES:
        :PHONE: %^{phone number}
        :ADDRESS: %^{Street name Street no., Postal Code Postal Area, Country}
        :BIRTHDAY: %^{yyyy-mm-dd}
        :EMAIL: %^{name@domain.com}
        :NOTE: %^{NOTE}
        :END:"))))

(use-package org-contacts
  :after org
  :custom (org-contacts-files '("~/Documents/notes/org/contacts.org")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (java . t)))

(setq org-confirm-babel-evaluate nil)
(org-babel-tangle-file "~/.emacs.d/init.org")

;; block templates
(setq org-structure-template-alist
      '(("l" . "src emacs-lisp")
        ("j" . "src java")
        ("s" . "src")
        ("e" . "example")
        ("q" . "quote")))

(use-package calfw
  :config
  ;; use swedish calendar
  (load "sv-kalender"))

;; integrate calfw with org
(use-package calfw-org
  :after calfw)

;; open calendar with two weeks view
(defun my/custom-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'two-weeks))

(use-package plantuml-mode
  :defer t
  :custom
  (org-plantuml-jar-path (expand-file-name "~/tools/plantuml/plantuml.jar")))

(use-package pdf-tools
  :mode "\\.pdf\\'"
  :custom
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  (pdf-tools-install))

;; .edn file type support
(use-package edn)

;; set language environment
(set-language-environment "UTF-8")

;; clean up unneccesary whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; map yes and no to y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; disable visual and audible bell
(setq ring-bell-function 'ignore)

;; increase large file warning threshold
(setq large-file-warning-threshold 100000000)

;; automatically reload files when changed
(global-auto-revert-mode t)

;; suppress auto revert messages
(setq auto-revert-verbose nil)

;; automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; add a newline automatically at the end of the file upon save
(setq require-final-newline t)

(use-package diminish
  :diminish visual-line-mode
  :diminish centered-window-mode
  :diminish eldoc-mode
  :diminish evil-collection-unimpaired-mode
  :diminish org-indent-mode
  :diminish abbrev-mode)
