;; -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :hook (;; Hide details
         dired-mode . dired-hide-details-mode)
  :custom
  ;; Hide message when omitting files
  (dired-omit-verbose nil)
  ;; Show hidden files, sort directories first
  (dired-listing-switches "-la --group-directories-first -v")
  ;; What files to hide in dired-omit-mode
  (dired-omit-files
   (rx (or (seq bol (? ".") "#")         ; emacs autosave files
           (seq bol "." (not (any "."))) ; dotfiles
           (seq "~" eol))))              ; backup files
  ;; No infinite dired buffers!
  (dired-kill-when-opening-new-dired-buffer t))

;; Use nerd icons in dired
(use-package nerd-icons-dired
  :diminish
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package browse-url
  :ensure nil
  :custom  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

(provide 'cfg-dired)
