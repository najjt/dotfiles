;; -*- lexical-binding: t; -*-

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.emacs.d/elisp/modules")

;; Add packages to load path
(add-to-list 'load-path '"~/.emacs.d/elisp/packages")

;; Load modules
(require 'm-startup)
(require 'm-ui)
(require 'm-dev)
(require 'm-dired)
(require 'm-help)
(require 'm-keys)
(require 'm-org)
(require 'm-pdf)
(require 'm-term)
(require 'm-mail)
(require 'm-rss)
(require 'm-text)
(require 'm-global-popup)
(require 'm-general)

(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)
