;; -*- lexical-binding: t; -*-

;; Core Settings

;; User information
(setq user-full-name "Martin Lönn Andersson"
      user-mail-address "martin@malon.se")

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Get environment variables from shell
(use-package exec-path-from-shell
  :config
  ;; Don't start an interactive shell (improves startup time)
  (setq exec-path-from-shell-arguments nil)

  ;; Import language, locale, and path
  (dolist (var '("LANG" "LC_ALL" "PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

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

;; sudo privileges
(use-package sudo-edit
  :defer t
  :diminish)

;; Increase large file warning threshold
(setq large-file-warning-threshold 100000000)

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

;; Use minibuffer whilst in the minibuffer
(setopt enable-recursive-minibuffers t)

;; Ensure new frames are focused
(defun my/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))
(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

;; Use Emacs minibuffer for GPG pinentry
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)

;; Refresh packages when using package-install
;; if last refresh was longer than 24 hours ago
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

;; Search and navigation commands
(use-package consult
  :defer nil
  :bind (("C-x C-r"   . consult-recent-file)
         ("C-s"       . consult-line)
         ("M-y"       . consult-yank-pop)
         ("C-c C-x f" . consult-recent-file))
  :custom (consult-preview-key "M-.") ; Preview manually with <M-.>
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)

  (use-package embark-consult))

;; Automatically follow symlinks without prompting
(setq vc-follow-symlinks t)

;; Copy links to system clipboard in terminal Emacs
(define-advice browse-url
    (:around (orig-fun &rest args) copy-url-if-termainl)
  (if (display-graphic-p)
      (apply orig-fun args)
    (let ((url (nth 0 args)))
      (message "Clipetty link: %s" url)
      (clipetty--emit (clipetty--osc url t)))))

;; Scroll just enough to bring cursor back into view
(setq scroll-conservatively 10000)

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

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  (("C-c f"     . find-name-dired)
   :map dired-mode-map
   ("b" . dired-up-directory))
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

(setq TeX-source-correlate-method 'synctex)

(use-package auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-mode 1)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(provide 'cfg-general)
