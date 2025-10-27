;; -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :custom
  ;; Show in long listing format,
  ;; show hidden files,
  ;; show sizes in human-readable format
  ;; sort directories first,
  ;; use natural sort for version numbers within text
  (dired-listing-switches "-lah --group-directories-first -v")

  ;; No infinite dired buffers!
  (dired-kill-when-opening-new-dired-buffer t))

;; Use nerd icons in dired...
(use-package nerd-icons-dired
  :diminish
  :hook ((dired-mode dired-after-readin). my/enable-nerd-icons-dired))

;; ... if in a graphical environment
(defun my/enable-nerd-icons-dired ()
  (when (display-graphic-p)
    (nerd-icons-dired-mode)
    (nerd-icons-dired--refresh)))

(use-package browse-url
  :ensure nil
  :custom  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

;; Show recursive directory size in dired
(use-package dired-du
  :custom (dired-du-size-format t)) ; Use human-readable format

(provide 'cfg-dired)
