;; -*- lexical-binding: t; -*-

(use-package mu4e
  :ensure nil
  :defer 20 ; Load 20 s after startup
  :commands (mu4e make-mu4e-context)
  :bind
  ("C-c m" . mu4e)

  (:map mu4e-view-mode-map
        ("e" . mu4e-view-save-attachment))
  :config
  (setq mail-user-agent 'mu4e-user-agent) ; Make mu4e default email client
  (set-variable 'read-mail-command 'mu4e) ; Make mu4e default email reader

  ;; User info
  (setq
   user-mail-address "martin@malon.se"
   user-full-name  "Martin Lönn Andersson")

  ;; Maildir setup
  (setq
   mu4e-maildir "~/.mail"
   mu4e-attachments-dir "~/Downloads"
   mu4e-refile-folder "/Archive"
   mu4e-sent-folder "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder "/Trash"

   mu4e-maildir-shortcuts
   '((:maildir "/INBOX"      :key ?i)
     (:maildir "/Viktigt"    :key ?v)
     (:maildir "/Orders"     :key ?r)
     (:maildir "/Resor"      :key ?k))

  ;; Fastmail likes to mark things trashed, so you have to
  ;; add filters against seeing them in standard searches
   mu4e-bookmarks
   `(("flag:unread AND NOT flag:trashed AND NOT maildir:/Spam" "Unread messages" ?u)
     ("date:today..now AND NOT flag:trashed AND NOT maildir:/Spam" "Today's messages" ?t)
     ("date:7d..now AND NOT flag:trashed AND NOT maildir:/Spam" "Last 7 days" ?w)))

  ;; Fetch mail
  (setq
   mu4e-get-mail-command "mbsync -a"
   mu4e-change-filenames-when-moving t   ; Needed for mbsync
   mu4e-update-interval 120)             ; Update every 2 minutes

  ;; Send mail
  (setq sendmail-program "/usr/bin/msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; Other options
  (setq
   mu4e-confirm-quit nil
   mu4e-headers-skip-duplicates t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-date-format "%d/%m/%y"
   mu4e-headers-date-format "%d/%m/%Y"
   mu4e-compose-dont-reply-to-self t
   mu4e-view-fields '(:from :to :cc :bcc :subject :flags :date :maildir :mailing-list :tags :attachments :signature :decryption)
   mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:from . 22) (:subject))

   ;; Re-flow mail so it's not hard wrapped
   mu4e-compose-format-flowed t

   ;; Hide annoying retrieving msg in mini buffer
   mu4e-hide-index-messages t
   mu4e-index-update-error-warning 'nil)

   ;; Move messages to the trash folder instead of completely deleting it
   (fset 'my-move-to-trash "mTrash")
   (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
   (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash))

(provide 'm-mail)
