;; -*- lexical-binding: t; -*-

(setq user-full-name "Martin Lönn Andersson"
      user-mail-address "martin@malon.se")

;;; Performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024))

;; Skip fontification during input
(setq redisplay-skip-fontification-on-input t)

;; Disable bidirectional text scanning
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Increase large file warning threshold
(setq large-file-warning-threshold 100000000)

;;; Customization & Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make customize-based setting live in the custom.el file
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; Save text entered in minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Persist kill ring across sessions
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))

;; Save place in files
(save-place-mode 1)

;; Remember recently edited files
(recentf-mode 1)

;;; File Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set backup, autosave and lockfiles
;; to live in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      lock-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Automatically reread files when changed
(setopt auto-revert-avoid-polling t
	auto-revert-interval 5
	auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Automatically reload non-file buffers
(setq global-auto-revert-non-file-buffers t)

;; Automatically follow symlinks without prompting
(setq vc-follow-symlinks t)

;; Auto chmod scripts on save
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

;; Recenter after save-place restores position
(advice-add 'save-place-find-file-hook :after
	    (lambda (&rest _)
	      (when buffer-file-name
		(ignore-errors (recenter)))))

;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set package archives
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
	("elpa"         . "https://elpa.gnu.org/packages/")
	("nongnu"       . "https://elpa.nongnu.org/nongnu/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("gnu-devel"    . "https://elpa.gnu.org/devel/")
	("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")))

;; Initialize the package manager
(package-initialize)

(setq use-package-always-ensure t
      use-package-verbose t)

;; Defer packages when not launched in daemon mode
(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer (not (daemonp))
      use-package-always-demand (daemonp))

;; Add packages to load path
(add-to-list 'load-path (expand-file-name "~/.config/emacs/lisp"))

;; Refresh packages when using package-install if last refresh was longer than 24 hours ago
;; Source: https://andreyor.st/posts/2022-07-15-refresh-package-contents-automatically/
(defcustom package-last-refresh-date nil
  "Date and time when package lists have been refreshed.

This variable is then used to check whether
`package-refresh-contents' call is needed before calling
`package-install'. The value of this variable is updated when
`package-refresh-contents' is called.

See `package-refresh-hour-threshold' for the amount of time needed to
trigger a refresh."
  :type 'string
  :group 'package)

(defcustom package-automatic-refresh-threshold 24
  "Amount of hours since last `package-refresh-contents' call
needed to trigger automatic refresh before calling `package-install'."
  :type 'number
  :group 'package)

(define-advice package-install (:before (&rest _) package-refresh-contents-maybe)
  (when (or (null package-last-refresh-date)
	    (> (/ (float-time
		   (time-subtract
		    (date-to-time (format-time-string "%Y-%m-%dT%H:%M"))
		    (date-to-time package-last-refresh-date)))
		  3600)
	       package-automatic-refresh-threshold))
    (package-refresh-contents)))

(define-advice package-refresh-contents (:after (&rest _) update-package-refresh-date)
  (customize-save-variable
   'package-last-refresh-date
   (format-time-string "%Y-%m-%dT%H:%M")))

;;; System Integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Use Emacs minibuffer for GPG pinentry
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)

;; Set default alert style to send desktop notifications
(setq alert-default-style 'libnotify)

;; Copy to system clipboard in terminal
(use-package clipetty
  :diminish
  :if (not (display-graphic-p))
  :hook (after-init . global-clipetty-mode))

;; Copy links to system clipboard in terminal Emacs
(define-advice browse-url
    (:around (orig-fun &rest args) copy-url-if-termainl)
  (if (display-graphic-p)
      (apply orig-fun args)
    (let ((url (nth 0 args)))
      (message "Clipetty link: %s" url)
      (clipetty--emit (clipetty--osc url t)))))

;;; General Behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Map yes and no to y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable visual and audible bell
(setq ring-bell-function 'ignore)

;; Suppress auto revert messages
(setq auto-revert-verbose nil)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; Use minibuffer whilst in the minibuffer
(setopt enable-recursive-minibuffers t)

;; Ensure new frames are focused
(defun my/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))

(add-hook 'server-after-make-frame-hook
	  #'my/focus-new-client-frame)

;; Show number of matches in the minibuffer
(setq isearch-lazy-count t)

;; Faster mark popping
(setq set-mark-command-repeat-pop t)

;; Delete selection on insert
(delete-selection-mode)

;; Save clipboard before killing
(setq save-interprogram-paste-before-kill t)

;; No duplicates in the kill ring
(setq kill-do-not-save-duplicates t)

;; Hide inapplicable commands for current major mode
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(setq mouse-wheel-scroll-amount '(4))

;;; Text-related Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clean up unnecessary whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(set-language-environment "UTF-8")

;; Automatic line breaking
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Spell checking
(use-package ispell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode))
  :bind ("C-c s" . flyspell-check-previous-highlighted-word)
  :config
  (setq ispell-program-name "hunspell"
	ispell-dictionary "sv_SE,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "sv_SE,en_US"))

;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easier repeating of commands
(repeat-mode 1)

;; Additional keybinding for M-x
(keymap-global-set "C-c k" 'execute-extended-command)

;; Increase/decrease text scale
(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "C-+" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)

;; Make escape quit prompts
(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; Keybind workarounds for terminal Emacs
(define-key key-translation-map (kbd "C-x ,") (kbd "C-x C-;"))
(define-key key-translation-map (kbd "C-c ,") (kbd "C-c C-,"))

(defun my/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(keymap-global-set "S-<return>" #'my/smart-open-line)

(keymap-global-set "M-g r" 'recentf)

(keymap-global-set "M-s g" 'grep)
(keymap-global-set "M-s r" 'rgrep)
(keymap-global-set "M-s f" 'find-name-dired)

;; Move forward to beginning of next word
(keymap-global-set "M-F" 'forward-to-word)

;; Move backward to end of previous word
(keymap-global-set "M-B" 'backward-to-word)

(keymap-global-set "M-o" 'other-window)

;;; Programming Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . (lambda ()
		       (subword-mode)
		       (show-paren-mode)
		       (electric-pair-mode))))

;; Git interface
(use-package magit
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-c g" . magit-status))

;; Show Git diff in margin
(use-package git-gutter
  :diminish
  :defer nil
  :config
  (global-git-gutter-mode))

;;; Common File Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode)
(use-package json-mode)
(use-package csv-mode)
(use-package markdown-mode)
(use-package lua-mode)
(use-package ini-mode)
(use-package rasi-mode)

;;; Completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :config
  (setq vertico-scroll-margin 0
	vertico-count 5
	vertico-resize t
	vertico-cycle t)
  (vertico-mode 1))

;; Better completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;; Completions
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  ;; Use <TAB> for both indentation & completion
  (setq tab-always-indent 'complete
	completion-cycle-threshold 1))

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
	      ("q" . (lambda ()
		       (interactive)
		       (quit-window))))
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

(use-package nerd-icons-corfu
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters
	       #'nerd-icons-corfu-formatter))

;;; Window Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Undo and redo changes in window layout
(winner-mode 1)

;; Make switching buffers more consistent
(setopt switch-to-buffer-obey-display-actions t
	switch-to-buffer-in-dedicated-window 'pop)

;; Automatically switch focus to new window when it is created
(defun my/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'my/split-and-follow-horizontally)

(defun my/split-and-follow-vertically ()
  (interactive)
  (split-window-right)n
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 3") 'my/split-and-follow-vertically)

;; Don't open a new window for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Open man pages in other window
(setq Man-notify-method 'aggressive)

;; Auto focus help-mode windows
(setq help-window-select t)

;; Proportionally resize windows
(setq window-combination-resize t)

;; Reversible <C-x 1>
(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
	   (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1")
		#'toggle-delete-other-windows)

(defun my/select-window (window &rest _)
  "Select WINDOW for display-buffer-alist"
  (select-window window))

;; Settings for displaying buffers
(setq display-buffer-alist
      '(((or . ((derived-mode . occur-mode)))
	 (display-buffer-reuse-mode-window
	  display-buffer-at-bottom)
	 (body-function . my/select-window)
	 (dedicated . t)
	 (preserve-size . (t . t)))

	((or . ((derived-mode . compilation-mode)))
	 (display-buffer-reuse-window
	  display-buffer-same-window)
	 (body-function . my/select-window)
	 (dedicated . t)
	 (inhibit-same-window . nil)
	 (preserve-size . (t . t)))

	((or . ((derived-mode . helpful-mode)))
	 (display-buffer-reuse-mode-window)
	 (dedicated . t))

	;; Hide compilation windows
	("\\*compilation\\*" display-buffer-no-window
	 (allow-no-window . t))))

;;; UI Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))

;; Show column number in status bar
(column-number-mode)

;; Show fringes
(fringe-mode 8)

;; Hide tab bar if only one tab
(setq tab-bar-show 1)

;; Make line numbers relative
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; Display line numbers in the below modes
(dolist (hook '(conf-mode-hook
		prog-mode-hook
		text-mode-hook
		markdown-mode-hook
		org-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;; Set font
(let ((mono-spaced-font "monospace")
      (proportionately-spaced-font "sans"))
  (set-face-attribute 'default nil
		      :family mono-spaced-font
		      :height 115)
  (set-face-attribute 'fixed-pitch nil
		      :family mono-spaced-font
		      :height 1.15)
  (set-face-attribute 'variable-pitch nil
		      :family proportionately-spaced-font
		      :height 1.1))

(defun my/disable-all-themes ()
  "Disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/enable-theme (theme)
  "Interactively enable the specified THEME and disable all other themes."
  (interactive
   (list
    (completing-read
     "Choose theme: "
     (mapcar #'symbol-name
	     (custom-available-themes)))))
  (my/disable-all-themes)
  (load-theme (intern theme) t)
  (customize-save-variable 'my-chosen-theme theme))

;; Remember last used theme between sessions
(add-hook 'after-init-hook
	  (lambda ()
	    (if (boundp 'my-chosen-theme)
		(my/enable-theme my-chosen-theme))))

;; Hide minor modes in modeline
(use-package diminish
  :diminish (auto-fill-function
	     centered-window-mode
	     eldoc-mode
	     evil-collection-unimpaired-mode
	     org-indent-mode
	     abbrev-mode
	     flymake-mode
	     hs-minor-mode))

(with-eval-after-load 'subword
  (diminish 'subword-mode))

;; Show full path to file in header line
(defun my/set-header-line-for-files-only ()
  "Set `header-line-format` to show file name info only in file-visiting buffers."
  (setq header-line-format
	(when buffer-file-name
	  (let ((file-name
		 (abbreviate-file-name buffer-file-name)))
	    (concat
	     (propertize file-name
			 'face 'italic))))))

(add-hook 'after-change-major-mode-hook
	  #'my/set-header-line-for-files-only)

;; Visualize color codes in text
(use-package colorful-mode
  :diminish
  :defer nil
  :config
  (global-colorful-mode t))

;; Enable context menu (mouse-click menu)
(dolist (hook '(text-mode-hook
		prog-mode-hook
		dired-mode-hook))
  (add-hook hook 'context-menu-mode))

;; Consider all themes as safe
(setq custom-safe-themes t)

;; Scroll just enough to bring cursor back into view
(setq scroll-conservatively 10000)

;; Don't render cursors in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;;; Org Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :bind (("C-c a"     . org-agenda)
	 ("C-c c"     . org-capture)
	 ("C-c l"     . org-store-link)
	 ("C-c M-RET" . org-insert-todo-heading))
  :config
  (setq org-directory "~/notes/org"
	org-todo-keywords '((sequence "TODO" "|" "DONE"))
	org-M-RET-may-split-line '((default . nil))
	org-insert-heading-respect-content t
	org-log-done 'time
	org-log-into-drawer t)

  (load "sv-kalender")

  ;; Refile settings
  (setq org-default-notes-file
	(concat org-directory "/omarkiv.org")
	org-refile-targets
	'(("todo.org"      :maxlevel . 2)
	  ("rep.org"       :maxlevel . 1)
	  ("arkiv.org"     :maxlevel . 1)
	  ("varia.org"     :maxlevel . 1)
	  ("kalender.org"  :level    . 0))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)

  ;; Appearance
  (setq org-tags-column 0
	org-startup-folded t
	org-src-preserve-indentation t
	;; Hide file names in agenda buffer
	org-agenda-prefix-format
	'((agenda . " %i %?-12t% s")
	  (todo   . " %i %-12:c")
	  (tags   . " %i %-12:c")
	  (search . " %i %-12:c"))
	org-ellipsis " ▾"
	;; Empty line before headings
	org-blank-before-new-entry
	'((heading . auto)
	  (plain-list-item . nil)))

  ;; Make only first org heading be bold
  (custom-set-faces
   '(org-level-1
     ((t (:inherit outline-1
		   :weight bold
		   :height 1.1))))
   '(org-level-2
     ((t (:inherit outline-2
		   :weight normal))))
   '(org-level-3
     ((t (:inherit outline-3
		   :weight normal))))
   '(org-level-4
     ((t (:inherit outline-4
		   :weight normal))))
   '(org-level-5
     ((t (:inherit outline-5
		   :weight normal))))
   '(org-level-6
     ((t (:inherit outline-6
		   :weight normal))))
   '(org-level-7
     ((t (:inherit outline-7
		   :weight normal))))
   '(org-level-8
     ((t (:inherit outline-8
		   :weight normal))))))

;; Generate a table of contents
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
	org-agenda-files '("~/notes/org")

	;; Time grid settings
	org-agenda-time-grid
	'((daily today require-timed remove-match)
	  (800 1000 1200 1400 1600 1800 2000)
	  "...."
	  "------------")
	org-agenda-current-time-string "← now")

  (setq org-agenda-custom-commands
	'(("w" "Week agenda"
	   ((agenda ""
		    ((org-agenda-span 'week)))))
	  ("k" "Month agenda"
	   ((agenda ""
		    ((org-agenda-span 'month)))))))

  ;; Date heading settings
  (custom-set-faces
   '(org-agenda-date
     ((t (:height 1.0
		   :weight bold
		   :background unspecified))))
   '(org-agenda-date-today
     ((t (:height 1.3
		   :weight bold
		   :background unspecified
		   :underline unspecified))))))

(use-package org-capture
  :ensure nil
  :after org
  :config
  ;; Don't save org capture bookmarks
  (setq org-bookmark-names-plist nil
	org-capture-bookmark nil)
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file "")
      "* TODO %?\n%i\n")

     ("l" "Task with link" entry (file "")
      "* TODO %?\n%i\n%a")

     ("n" "Note" entry (file "")
      "* %?\n%i\n")

     ("e" "Calendar event" entry (file "kalender.org")
      "* %?\n%^t")

     ("c" "Contact" entry (file "kontakter.org")
      "* %? \n:PROPERTIES:\n:PHONE: %^{Phone number}\n:END:"))))

(use-package org-contacts
  :after org
  :pin melpa
  :custom
  (org-contacts-files '("~/notes/org/kontakter.org")))

(use-package org-wild-notifier
  :config
  (org-wild-notifier-mode)
  (setq org-wild-notifier-day-wide-alert-times
	'("07:00")))

;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

;; Open multiple vterm buffers
(use-package multi-vterm
  :bind
  ("C-c T" . multi-vterm-dedicated-toggle)
  ("C-c t" . multi-vterm)
  :config
  (setq multi-vterm-dedicated-window-height-percent 30))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map
	("b" . dired-up-directory))
  :custom
  ;; Show in long listing format,
  ;; show hidden files,
  ;; show sizes in human-readable format
  ;; sort directories first,
  ;; use natural sort for version numbers within text
  (dired-listing-switches
   "-lAhv --group-directories-first")
  ;; No infinite dired buffers!
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-open)

(use-package auctex
  :config
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-view-program-selection
	'((output-pdf "PDF Tools"))
	TeX-view-program-list
	'(("PDF Tools"
	   TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t
	TeX-source-correlate-mode 1)
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer))

(use-package pdf-tools)

;; Local Variables:
;; outline-minor-mode-cycle: t
;; outline-regexp: ";;; "
;; eval: (outline-minor-mode)
;; eval: (outline-hide-body)
;; End:
