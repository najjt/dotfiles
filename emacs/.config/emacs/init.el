;; -*- lexical-binding: t; -*-

(setq native-comp-jit-compilation-deny-list '(".*org-element.*"))

;; Performance
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024))

;; Make customize-based setting live in the custom.el file
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; Set backup, autosave and lockfiles
;; to live in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      lock-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(eval-when-compile (require 'use-package))
(setq use-package-always-defer (not (daemonp))
      use-package-always-demand (daemonp))

;; Add packages to load path
(add-to-list 'load-path (expand-file-name "~/.config/emacs/elisp"))

;; Load setup files
(mapc 'load (file-expand-wildcards "~/.config/emacs/elisp/modules/*.el"))

;; Display startup time
(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		   (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)
