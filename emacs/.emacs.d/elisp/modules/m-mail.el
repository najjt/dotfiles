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
     (:maildir "/Resor"      :key ?k)))

  ;; Fetch mail
  (setq
   mu4e-get-mail-command "mbsync -a"
   mu4e-change-filenames-when-moving t   ; Needed for mbsync
   mu4e-update-interval 120)             ; Update every 2 minutes

  ;; Send mail
  (setq
   message-send-mail-function 'smtpmail-send-it
   smtpmail-auth-credentials "~/.authinfo.gpg"
   smtpmail-default-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-server "smtp.fastmail.com")

  ;; Other options
  (setq
   mu4e-confirm-quit nil
   mu4e-headers-skip-duplicates t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-date-format "%d/%m/%y"
   mu4e-headers-date-format "%d/%m/%Y"

   ;; Re-flow mail so it's not hard wrapped
   mu4e-compose-format-flowed t

   ;; Hide annoying retrieving msg in mini buffer
   mu4e-hide-index-messages t
   mu4e-index-update-error-warning 'nil)

   ;; Move messages to the trash folder instead of completely deleting it
   ;; (fset 'my-move-to-trash "mTrash")
   ;; (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
   ;; (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash))
  )

(provide 'm-mail)
