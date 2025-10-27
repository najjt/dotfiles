;; -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :hook (org-mode . (lambda () (setq tab-width 8)))
  :config
  ;; Basic settings
  (setq org-directory "~/notes/org"
        org-todo-keywords '((sequence "TODO" "|" "DONE"))
        org-M-RET-may-split-line '((default . nil))
        org-insert-heading-respect-content t
        org-log-done 'time
        org-log-into-drawer t)

  (load "sv-kalender")

  ;; Refile settings
  (setq org-default-notes-file (concat org-directory "/refile.org")
        org-refile-targets (quote (("todo.org"          :maxlevel . 2)
                                   ("arkiv.org"         :maxlevel . 2)
                                   ("repeat.org"        :maxlevel . 2)
                                   ("misc.org"          :maxlevel . 1)
                                   ("kalender.org"      :level    . 0)))
        org-refile-use-outline-path 'file       ; Provide refile targets as path
        org-outline-path-complete-in-steps nil) ; Refile in a single go

  ;; Appearance
  (setq org-tags-column 0               ; Position tags next to heading
        org-startup-folded t
        org-src-preserve-indentation t  ; Don't add unnecessary indentation
        org-ellipsis " ▾"
        ;; Empty line before headings
        org-blank-before-new-entry (quote ((heading . auto)
                                           (plain-list-item . nil))))

  ;; Make only first org heading be bold
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :weight bold :height 1.1))))
   '(org-level-2 ((t (:inherit outline-2 :weight normal))))
   '(org-level-3 ((t (:inherit outline-3 :weight normal))))
   '(org-level-4 ((t (:inherit outline-4 :weight normal))))
   '(org-level-5 ((t (:inherit outline-5 :weight normal))))
   '(org-level-6 ((t (:inherit outline-6 :weight normal))))
   '(org-level-7 ((t (:inherit outline-7 :weight normal))))
   '(org-level-8 ((t (:inherit outline-8 :weight normal)))))

  ;; Put cursor at beginning of buffer when opening agenda
  (add-hook 'org-agenda-finalize-hook #'beginning-of-buffer))

;; Generate a table of contents
(use-package toc-org
  :defer t
  :hook (org-mode . toc-org-mode))

(use-package org-agenda
  :ensure nil
  :after org
  :config
  (setq org-agenda-span 'day
        org-agenda-tags-column 0
        org-agenda-start-on-weekday nil
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-todo-list-sublevels t
        org-agenda-files '("~/notes/org")

        ;; Time grid settings
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "...." "------------")
        org-agenda-current-time-string "← now")

  (setq org-agenda-custom-commands
        '(("w" "Week agenda"
           ((agenda "" ((org-agenda-span 'week)))))))

  ;; Date heading settings
  (custom-set-faces
   '(org-agenda-date ((t (:height 1.0 :weight bold :background unspecified))))
   '(org-agenda-date-today ((t (:height 1.3 :weight bold :background unspecified :underline unspecified))))))

;; Show org item properties in the agenda buffer
(use-package org-agenda-property)

(use-package org-capture
  :ensure nil
  :after org
  :config
  ;; Don't save org capture bookmarks
  (setq org-bookmark-names-plist nil
        org-capture-bookmark nil)
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file "")
      "* TODO %?\n%i\n")

     ("l" "Task with link" entry (file "")
      "* TODO %?\n%i\n%a")

     ("n" "Note" entry (file "")
      "* %?\n%i\n")

     ("e" "Calendar event" entry (file "calendar.org")
      "* %?\n%^t")

     ("c" "Contact" entry (file "contacts.org")
      "* %? \n:PROPERTIES:\n:PHONE: %^{Phone number}\n:END:"))))

(use-package org-contacts
  :after org
  :pin melpa
  :defer t
  :custom (org-contacts-files '("~/notes/org/kontakter.org")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (java . t)))

(setq org-confirm-babel-evaluate nil)

;; Block templates
(setq org-structure-template-alist
      '(("l" . "src emacs-lisp")
        ("j" . "src java")
        ("s" . "src")
        ("e" . "example")
        ("q" . "quote")))

;; Generate mind maps from org files
(use-package org-mind-map
  :init
  (require 'ox-org))

(provide 'cfg-org)
