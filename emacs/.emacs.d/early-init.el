;; -*- lexical-binding: t; -*-

;; Increase garbace collection threshold temporarily
(setq gc-cons-threshold 100000000)

;; Prevent Emacs from initializing package manager,
;; as I do that later on manually
(setq package-enable-at-startup nil)

;; Unset file-name-handler-alist temporarily
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore original file-name-handler-alist after initialization
(add-hook 'after-init-hook
          (lambda () (setq file-name-handler-alist file-name-handler-alist-original)))

;; Hide UI elements
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; Don't show unnecessary warnings
(setq warning-minimum-level :error
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;; Show timestamps in *Messages* buffer
;; Source: https://emacs.stackexchange.com/a/38511
(defun my/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
        Activate this advice with:
          (advice-add 'message :before 'my/ad-timestamp-message)
        Deactivate this advice with:
          (advice-remove 'message 'my/ad-timestamp-message)"
  (if message-log-max
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (format-time-string "[%F %T.%3N] "))))))

(advice-add 'message :before 'my/ad-timestamp-message)
