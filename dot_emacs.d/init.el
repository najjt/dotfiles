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
(require 'm-general)
(require 'm-help)
(require 'm-keys)
(require 'm-org)
(require 'm-pdf)
(require 'm-qol)
(require 'm-term)
