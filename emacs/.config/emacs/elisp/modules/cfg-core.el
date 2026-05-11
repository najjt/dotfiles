;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; User information
(setq user-full-name "Martin Lönn Andersson"
      user-mail-address "martin@malon.se")

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Save text entered in minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Save place in files
(save-place-mode 1)

;; Remember recently edited files
(recentf-mode 1)

;; Automatically reread files when changed
(setopt auto-revert-avoid-polling t
	auto-revert-interval 5
	auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Automatically reload non-file buffers
(setq global-auto-revert-non-file-buffers t)

;; Increase large file warning threshold
(setq large-file-warning-threshold 100000000)

;; Clean up unnecessary whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

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
(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

;; Use Emacs minibuffer for GPG pinentry
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)

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
		   (time-subtract (date-to-time (format-time-string "%Y-%m-%dT%H:%M"))
				  (date-to-time package-last-refresh-date)))
		  3600)
	       package-automatic-refresh-threshold))
    (package-refresh-contents)))

(define-advice package-refresh-contents (:after (&rest _) update-package-refresh-date)
  (customize-save-variable 'package-last-refresh-date
			   (format-time-string "%Y-%m-%dT%H:%M")))

;; Set default alert style to send desktop notifications
(setq alert-default-style 'libnotify)

;; Automatically follow symlinks without prompting
(setq vc-follow-symlinks t)

;; Show number of matches in the minibuffer
(setq isearch-lazy-count t)

;; Auto chmod scripts on save
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

;; Faster mark popping
(setq set-mark-command-repeat-pop t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text-related Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment "UTF-8")

;; Disable bidirectional text scanning
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Automatic line breaking
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Delete selection on insert
(delete-selection-mode)

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

;; Copy to system clipboard in terminal
(use-package clipetty
  :diminish
  :if (not (display-graphic-p))
  :hook (after-init . global-clipetty-mode))

;; Skip fontification during input
(setq redisplay-skip-fontification-on-input t)

;; Save clipboard before killing
(setq save-interprogram-paste-before-kill t)

;; No duplicates in the kill ring
(setq kill-do-not-save-duplicates t)

;; Persist kill ring across sessions
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))

;; Hide inapplicable commands for current major mode
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Additional keybinding for M-x
(keymap-global-set "C-c k" 'execute-extended-command)

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

(keymap-global-set "S-<return>" #'my/smart-open-line)

(keymap-global-set "M-g r" 'recentf)

(keymap-global-set "M-s g" 'grep)
(keymap-global-set "M-s r" 'rgrep)
(keymap-global-set "M-s f" 'find-name-dired)

;; Move forward to beginning of next word
(keymap-global-set "M-F" 'forward-to-word)
;; Move backward to end of previous word
(keymap-global-set "M-B" 'backward-to-word)

;; Easier repeating of commands
(repeat-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . (lambda ()
		       (subword-mode)         ; Toggle subword movement
		       (show-paren-mode)      ; Highlight matching parentheses
		       (electric-pair-mode))) ; Insert matching delimiters
  :mode ("\\.rasi\\'"
	 "\\.edn\\'"))

;; Git interface
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-c g" . magit-status))

;; Show Git diff in margin
(use-package git-gutter
  :diminish
  :defer nil
  :config
  (global-git-gutter-mode))

;; Common file types
(use-package yaml-mode)
(use-package json-mode)
(use-package csv-mode)
(use-package markdown-mode)
(use-package lua-mode)
(use-package ini-mode)
(use-package rasi-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completions
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal/Console Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copy links to system clipboard in terminal Emacs
(define-advice browse-url
    (:around (orig-fun &rest args) copy-url-if-termainl)
  (if (display-graphic-p)
      (apply orig-fun args)
    (let ((url (nth 0 args)))
      (message "Clipetty link: %s" url)
      (clipetty--emit (clipetty--osc url t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell/Vterm Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (vterm-shell "zsh")
  (vterm-max-scrollback 10000))

;; Open multiple vterm buffers
(use-package multi-vterm
  :bind
  ("C-c T"     . multi-vterm-dedicated-toggle)
  ("C-c t" . multi-vterm)
  :config
  (setq multi-vterm-dedicated-window-height-percent 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File/Directory Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map ("b" . dired-up-directory))
  :custom
  ;; Show in long listing format,
  ;; show hidden files,
  ;; show sizes in human-readable format
  ;; sort directories first,
  ;; use natural sort for version numbers within text
  (dired-listing-switches "-lAhv --group-directories-first")
  ;; No infinite dired buffers!
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-open)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX/LaTeX Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auctex
  :config
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t
	TeX-source-correlate-mode 1)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(provide 'cfg-core)
