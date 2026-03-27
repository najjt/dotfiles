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

(use-package dired-open)

(provide 'cfg-dired)
