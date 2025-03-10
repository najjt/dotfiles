;; -*- lexical-binding: t; -*-

;; During early initialization, I increase the gc-cons-threshold
;; temporarily to speed up the start up process. However, a large
;; gc-cons-threshold may cause freezing and stuttering during long-term
;; interactive use, so here I set it to a more stable setting
(defvar better-gc-cons-threshold 4294967269 ; 512mb
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
      (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))

;; Garbage Collect when Emacs is out of focus and avoid garbage
;; collection when using the minibuffer
(add-hook 'emacs-startup-hook
      (lambda ()
        (if (boundp 'after-focus-change-function)
        (add-function :after after-focus-change-function
                  (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))
          (add-hook 'after-focus-change-function 'garbage-collect))
        (defun gc-minibuffer-setup-hook ()
          (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

        (defun gc-minibuffer-exit-hook ()
          (garbage-collect)
          (setq gc-cons-threshold better-gc-cons-threshold))

        (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
        (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;; Make customize-based setting live in the custom.el file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Set backup, autosave and lockfiles
;; to live in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq lock-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")))

;; Initialize the package manager
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; Add packages to load path
(add-to-list 'load-path '"~/.emacs.d/elisp/packages")

;; Load setup files in .emacs.d/elisp/modules/
(mapc 'load (file-expand-wildcards "~/.emacs.d/elisp/modules/*.el"))

;; Focus new frames
(defun my/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))

(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)
