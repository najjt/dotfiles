;; -*- lexical-binding: t; -*-

;; During early initialization, I increase the gc-cons-threshold temporarily
;; to speed up the start up process. However, a large gc-cons-threshold may
;; cause freezing and stuttering during long-term interactive use, so here I
;; set it to a more stable setting
(defvar better-gc-cons-threshold 4294967269 ; 512mb
  "The default value to use for `gc-cons-threshold'.

  If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold better-gc-cons-threshold)))


;; Garbage Collect when Emacs is out of focus and avoid garbage collection
;; when using the minibuffer
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

;; Define constants for detecting the current OS
(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;; Linux settings
(when *sys/linux*
  (setq x-super-keysym 'meta)
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; macOS settings
(when *sys/mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq frame-resize-pixelwise t)
  (global-set-key (kbd "C-x C-z") 'ns-do-hide-emacs))

;; Make customize-based setting live in the custom.el file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Set the backup directory location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Start the Emacs server if it isn't already running
(load "server")
(unless (server-running-p)
    (server-start))

(provide 'ml-start)
