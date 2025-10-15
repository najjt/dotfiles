;; -*- lexical-binding: t; -*-

(use-package mu4e
  :commands (mu4e make-mu4e-context)
  :load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.8.14"
  :bind
  ("C-c m" . mu4e)
  (:map mu4e-compose-mode-map
        ("C-c C-a" . mail-add-attachment))
  (:map mu4e-view-mode-map
        ("e"       . mu4e-view-save-attachment))
  :config
  (setq mail-user-agent 'mu4e-user-agent) ; Make mu4e default email client
  (set-variable 'read-mail-command 'mu4e) ; Make mu4e default email reader

  ;; Maildir setup
  (setq
   mu4e-maildir        "~/.mail"
   mu4e-attachment-dir "~/Downloads"
   mu4e-refile-folder  "/Archive"
   mu4e-sent-folder    "/Sent"
   mu4e-drafts-folder  "/Drafts"
   mu4e-trash-folder   "/Trash"

   mu4e-maildir-shortcuts
   '((:maildir "/INBOX"   :key ?i)
     (:maildir "/Sent"    :key ?s)
     (:maildir "/Drafts"  :key ?d)
     (:maildir "/Trash"   :key ?t)
     (:maildir "/Archive" :key ?a)
     (:maildir "/Viktigt" :key ?v))

   ;; Filters for hiding trashed messages from bookmarks
   mu4e-bookmarks
   `(("flag:unread AND NOT flag:trashed AND NOT maildir:/Spam AND NOT maildir:/Trash" "Unread messages" ?u)
     ("date:today..now AND NOT flag:trashed AND NOT maildir:/Spam AND NOT maildir:/Trash" "Today's messages" ?t)
     ("maildir:/INBOX AND NOT flag:trashed AND NOT maildir:/Spam AND NOT maildir:/Trash" "Inbox" ?i)
     ("date:7d..now AND NOT flag:trashed AND NOT maildir:/Spam AND NOT maildir:/Trash" "Last 7 days" ?w)))

  ;; Fetch mail
  (setq
   mu4e-get-mail-command "mbsync -a"
   mu4e-change-filenames-when-moving t   ; Needed for mbsync
   mu4e-update-interval 300)             ; Update every 5 minutes

  ;; Send mail
  (setq sendmail-program "/usr/bin/msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "main"
             :enter-func (lambda () (mu4e-message "Entering MAIN context"))
             :leave-func (lambda () (mu4e-message "Leaving MAIN context"))
             :vars '((user-mail-address     . "martin@malon.se")
                     (user-full-name        . "Martin Lönn andersson")
                     (message-signature     .
                                            (concat
                                             "Med vänlig hälsning,\n"
                                             "Martin Lönn Andersson\n"))))
           ,(make-mu4e-context
             :name "Mask"
             :enter-func (lambda () (mu4e-message "Switch to the MASK context"))
             ;; no leave-func
             :vars '((user-mail-address      . "tidy.dust2080@fastmail.com")
                     (user-full-name         . "Martin")
                     (message-signature      .
                                             (concat
                                              "Kind regards,\n"
                                              "Martin\n"))))
           ,(make-mu4e-context
             :name "Other mask"
             :enter-func (lambda () (mu4e-message "Switch to the OTHER MASK context"))
             ;; no leave-func
             :vars '((user-mail-address      . "kind.sand2320@fastmail.com")
                     (user-full-name         . "Martin")
                     (message-signature      .
                                             (concat
                                              "Kind regards,\n"
                                              "Martin\n"))))))

  ;; start with the first (default) context
  (setq mu4e-context-policy 'pick-first)

  ;; Other options
  (setq
   mu4e-confirm-quit nil
   mu4e-headers-skip-duplicates t
   mu4e-display-update-status-in-modeline t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-date-format "%d/%m/%y"
   mu4e-headers-date-format "%d/%m/%Y"
   mu4e-compose-dont-reply-to-self t
   mu4e-view-fields '(:from :to :cc :bcc :subject :flags :date :maildir :mailing-list :tags :attachments :signature :decryption)
   mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:from . 22) (:subject))

   ;; Re-flow mail so it's not hard wrapped
   mu4e-compose-format-flowed t
   ;; The column beyond which flowed lines are wrapped
   fill-flowed-encode-column 80)

  ;; Move messages to the trash folder instead of completely deleting it
  (fset 'my/move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'my/move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my/move-to-trash)

  ;; Update on quitting
  (defun my/mu4e-quit ()
    (interactive)
    (mu4e-update-mail-and-index t)
    (mu4e-quit))

  (with-eval-after-load 'mu4e
    (evil-define-key 'normal mu4e-main-mode-map (kbd "q") #'my/mu4e-quit))

  ;; Prefer plain text
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; Use org mode to send HTML emails
(use-package org-mime
  :bind (:map mu4e-compose-mode-map
              ("C-c C-o" . org-mime-edit-mail-in-org-mode)
              ("C-c h"   . org-mime-htmlize))
  :config
  ;; Ask before sending email if it should be HTML
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))

;; Add attachments from dired
(use-package gnus-dired
  :ensure nil
  :after message
  :hook
  (dired-mode . turn-on-gnus-dired-mode))

(provide 'cfg-mail)
