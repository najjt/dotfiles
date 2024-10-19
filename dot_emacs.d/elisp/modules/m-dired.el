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

;; Use nerd icons in dired
(use-package nerd-icons-dired
  :diminish
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'm-dired)
