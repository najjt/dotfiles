;; -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  ;; Show in long listing format,
  ;; show hidden files,
  ;; show sizes in human-readable format
  ;; sort directories first,
  ;; use natural sort for version numbers within text
  (dired-listing-switches "-lAhv --group-directories-first")

  ;; No infinite dired buffers!
  (dired-kill-when-opening-new-dired-buffer t))

(use-package browse-url
  :ensure nil
  :custom (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

;; Show recursive directory size in dired
(use-package dired-du
  :custom (dired-du-size-format t)) ; Use human-readable format

(provide 'cfg-dired)
