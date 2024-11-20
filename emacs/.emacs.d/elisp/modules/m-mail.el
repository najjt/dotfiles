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
  (add-to-list 'gnutls-trustfiles (expand-file-name "~/.config/protonmail/bridge/cert.pem"))

  (setq mail-user-agent 'mu4e-user-agent) ; mu4e default email client
  (set-variable 'read-mail-command 'mu4e) ; mu4e default email reader

  (setq
   ;; User info
   user-mail-address "mlonna@pm.me"
   user-full-name  "Martin Lönn Andersson"

   ;; Maildir setup
   mu4e-root-maildir "~/.mail"
   mu4e-attachment-dir "~/Downloads"
   mu4e-maildir-shortcuts
   '((:maildir "/Proton/Inbox"                :key ?i)
     (:maildir "/Proton/Folders/Viktigt"      :key ?v)
     (:maildir "/Proton/Folders/Orders"       :key ?r)
     (:maildir "/Proton/Folders/Kvitton"      :key ?k))

   mu4e-contexts
   `(,(make-mu4e-context
       :name "mlonna"
       :match-func
       (lambda (msg)
         (when msg
           (mu4e-message-contact-field-matches msg
                                               :to "mlonna@pm.me")))
       :vars '((user-mail-address . "mlonna@pm.me" )
               (user-full-name . "Martin Lönn Andersson")
               (mu4e-drafts-folder . "/Proton/Drafts")
               (mu4e-sent-folder . "/Proton/Sent")
               (mu4e-refile-folder . "/Proton/Archive")
               (mu4e-trash-folder . "/Proton/Trash")))

     ,(make-mu4e-context
       :name "nitramla"
       :match-func
       (lambda (msg)
         (when msg
           (mu4e-message-contact-field-matches msg
                                               :to "nitramla@pm.me")))
       :vars '((user-mail-address . "nitramla@pm.me")
               (user-full-name . "Martin")
               (mu4e-drafts-folder . "/Proton/Drafts")
               (mu4e-sent-folder . "/Proton/Sent")
               (mu4e-refile-folder . "/Proton/Archive")
               (mu4e-trash-folder . "/Proton/Trash")))

     ,(make-mu4e-context
       :name "hemlg"
       :match-func
       (lambda (msg)
         (when msg
           (mu4e-message-contact-field-matches msg
                                               :to "hemlg@pm.me")))
       :vars '((user-mail-address . "hemlg@pm.me")
               (user-full-name . "Martin")
               (mu4e-drafts-folder . "/Proton/Drafts")
               (mu4e-sent-folder . "/Proton/Sent")
               (mu4e-refile-folder . "/Proton/Archive")
               (mu4e-trash-folder . "/Proton/Trash")))

     ,(make-mu4e-context
       :name "trshcan"
       :match-func
       (lambda (msg)
         (when msg
           (mu4e-message-contact-field-matches msg
                                               :to "trshcan@pm.me")))
       :vars '((user-mail-address . "trshcan@pm.me")
               (user-full-name . "Martin")
               (mu4e-drafts-folder . "/Proton/Drafts")
               (mu4e-sent-folder . "/Proton/Sent")
               (mu4e-refile-folder . "/Proton/Archive")
               (mu4e-trash-folder . "/Proton/Trash"))))

   ;; Start with the first (default) context
   mu4e-context-policy 'pick-first

   ;; Ask for context if no context matches
   mu4e-compose-context-policy 'ask

   ;; Fetch mail
   mu4e-get-mail-command "mbsync -a"
   mu4e-change-filenames-when-moving t   ; Needed for mbsync
   mu4e-update-interval 120              ; Update every 2 minutes

   ;; Send mail
   message-send-mail-function 'message-send-mail-with-sendmail
   smtpmail-auth-credentials "~/.authinfo"
   smtpmail-smtp-server "127.0.0.1"
   smtpmail-smtp-service 1025
   smtpmail-stream-type 'starttls

   ;; Other options
   mu4e-confirm-quit nil

   ;; Re-flow mail so it's not hard wrapped
   mu4e-compose-format-flowed t

   ;; Hide annoying retrieving msg in mini buffer
   mu4e-hide-index-messages t
   mu4e-index-update-error-warning 'nil))

(provide 'm-mail)
