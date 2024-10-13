(defvar better-gc-cons-threshold 4294967269 ; 512mb
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
  (setq x-super-keysym 'meta)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(when *sys/mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq frame-resize-pixelwise t)
  ;; (add-to-list 'default-frame-alist '(undecorated . t))
  (global-set-key (kbd "C-x C-z") 'ns-do-hide-emacs)
  ;; Start server if it isn't running
  (load "server")
  (unless (server-running-p)
    (server-start)))

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

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq
 use-package-always-ensure t
 use-package-verbose t)

;; Increase large file warning threshold
(setq large-file-warning-threshold 100000000)

;; Set language environment
(set-language-environment "UTF-8")

;; Clean up unneccesary whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Map yes and no to y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable visual and audible bell
(setq ring-bell-function 'ignore)

;; Suppress auto revert messages
(setq auto-revert-verbose nil)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; Add a newline automatically at the end of the file upon save
(setq require-final-newline t)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; Make switching buffers more consistent
(setopt switch-to-buffer-obey-display-actions t)

;; Smooth scrolling
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

;; Don't open a new window for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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

(defun my-display-org-agenda-in-new-frame (frame)
  "Display the Org agenda for today in new frames."
  (with-selected-frame frame
    (when (display-graphic-p)  ;; Ensure it's a graphical frame
      (org-agenda nil "a"))))  ;; "a" is the default key for agenda view

(add-hook 'after-make-frame-functions #'my-display-org-agenda-in-new-frame)

(use-package vertico
  :init
  (vertico-mode))

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

;; Save text entered in minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember recently edited files
(recentf-mode 1)

;; Automatically reread files when changed
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Auto reload non-file buffers
(setq global-auto-revert-non-file-buffers t)

(use-package exec-path-from-shell
  :config
  ;; Don't start an interactive shell (improves startup time)
  (setq exec-path-from-shell-arguments nil)
  ;; Which environment variables to import
  (dolist (var '("LANG" "LC_ALL"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package dired
  :ensure nil
  :hook (dired-mode . (lambda ()
                        (dired-hide-details-mode) ; Hide details by default
                        (dired-omit-mode)))       ; Hide hidden files
  :bind (("C-x C-j" . dired-jump)
         :map evil-normal-state-map
         ("z d" . dired-hide-details-mode)        ; Toggle details
         ("z h" . dired-omit-mode))               ; Toggle details
  :custom
  (dired-free-space nil)                          ; Hide free space
  (dired-omit-verbose nil)                        ; Hide message when omitting files
  :config
  (when *sys/mac*
    ;; Set directory program to gls on macOS
    ;; since flag --group-directories-first
    ;; doesn't exist on macOS' stock ls
    (setq insert-directory-program "gls")
    ;; Don't use --dired flag with ls on macOS
    (setq dired-use-ls-dired nil))

  ;; Show hidden files, sort directories first
  (setq dired-listing-switches "-la --group-directories-first -v")

  ;; What files to hide in dired-omit-mode
  (setq dired-omit-files
        (rx (or (seq bol (? ".") "#")         ; emacs autosave files
                (seq bol "." (not (any "."))) ; dot-files
                (seq "~" eol)                 ; backup-files
                )))

  ;; No infinite dired buffers!
  (setq dired-kill-when-opening-new-dired-buffer t))

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
          "^\\*Buffer List*\\*$"
          comint-mode
          eshell-mode
          shell-mode
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

;; Display right and left fringe
(fringe-mode '(8 . 8))

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Show column number in status bar
(column-number-mode)

;; Disable border around modelines
(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))

;; Make line numbers relative
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; Display relative line numbers in the below modes
(dolist (hook '(fundamental-mode conf-mode-hook prog-mode-hook text-mode-hook markdown-mode-hook org-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;; Highlight current line
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook dired-mode-hook Man-mode-hook conf-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Make keybindings in minibuffer look like other text
(set-face-attribute 'help-key-binding nil
                    :box nil
                    :foreground "unspecified"
                    :background "unspecified"
                    :inherit nil)

(when *sys/linux*
  (add-to-list 'default-frame-alist '(font . "Terminus (TTF)-11")))

(when *sys/mac*
  (add-to-list 'default-frame-alist '(font . "Terminus (TTF)-18")))

(use-package modus-themes
  :defer t)

(load-theme 'modus-vivendi)

(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 72)

(use-package vundo)

(setq-default tab-width 4 indent-tabs-mode nil)

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

(use-package org
  :pin nongnu
  :ensure org-contrib ; Needed for org-contacts
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config

  ;; Basic settings
  (setq org-directory "~/notes/org"
        org-default-notes-file (concat org-directory "/refile.org")
        org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))
        org-refile-targets (quote (("backlog.org"             :maxlevel   . 2)
                                   ("../misc/computer.org"    :maxlevel   . 1)
                                   ("../misc/inköpslista.org" :maxlevel   . 1)
                                   ("privat.org"              :maxlevel   . 2)
                                   ("studier.org"             :maxlevel   . 2)))
        org-outline-path-complete-in-steps nil ; Refile in a single go
        org-refile-use-outline-path t)         ; Show full paths for refiling

  ;; Refile between files
  ;; (for some reason I had to put this setting
  ;; here, by itself, for it to work)
  (setq org-refile-use-outline-path 'file)

  ;; Appearance
  (setq org-tags-column 0                                   ; Position tags next to heading
        org-startup-folded t
        org-blank-before-new-entry (quote ((heading . auto) ; Empty line before headings
                                           (plain-list-item . nil)))
        org-src-preserve-indentation t)                     ; Don't add unnecessary indentation

  ;; Make only first org heading be bold
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :weight normal))))
   '(org-level-3 ((t (:inherit outline-3 :weight normal))))
   '(org-level-4 ((t (:inherit outline-4 :weight normal))))
   '(org-level-5 ((t (:inherit outline-5 :weight normal))))
   '(org-level-6 ((t (:inherit outline-6 :weight normal))))
   '(org-level-7 ((t (:inherit outline-7 :weight normal))))
   '(org-level-8 ((t (:inherit outline-8 :weight normal))))))

;; Generate a table of contents
(use-package toc-org
  :defer t
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
        org-element-use-cache nil              ; org element cache often produced errors, so I disabled it
        org-agenda-scheduled-leaders '("" "")  ; Hide "Scheduled" text
        org-agenda-prefix-format "  %?-12t% s" ; Hide category for agenda items

        org-agenda-files '("~/notes/org")
        ;; Add newline above date heading
        org-agenda-format-date
        (lambda (date)
          (concat "\n" (org-agenda-format-date-aligned date)))

        ;; Time grid settings
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "...." "------------")
        org-agenda-current-time-string
        "← now")

  (setq org-agenda-custom-commands
        '(("w" "Week agenda"
           ((agenda "" ((org-agenda-span 'week)))))))

  ;; Date heading settings
  (custom-set-faces
   '(org-agenda-date ((t (:height 1.0 :weight bold :background "unspecified"))))
   '(org-agenda-date-today ((t (:height 1.3 :weight bold :background "unspecified" :underline nil))))))

(use-package org-capture
  :ensure nil
  :after org
  :config
  ;; Don't save org capture bookmarks
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
  :defer t
  :custom (org-contacts-files '("~/notes/org/contacts.org")))

(use-package calfw
  :config
  ;; Use Swedish calendar
  (load "sv-kalender"))

;; Integrate calfw with org
(use-package calfw-org
  :after calfw)

(defun my/custom-open-calendar ()
  "Open calendar with two weeks view"
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'two-weeks))

(use-package diminish
  :diminish (auto-fill-function
             centered-window-mode
             eldoc-mode
             evil-collection-unimpaired-mode
             org-indent-mode
             abbrev-mode))
