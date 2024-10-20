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
        org-default-notes-file (concat org-directory "/refile.org")
        org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))
        org-refile-targets (quote (("backlog.org"             :maxlevel   . 2)
                                   ("../misc/computer.org"    :maxlevel   . 1)
                                   ("../misc/inköpslista.org" :maxlevel   . 1)
                                   ("privat.org"              :maxlevel   . 2)
                                   ("studier.org"             :maxlevel   . 2)))
        org-outline-path-complete-in-steps nil ; Refile in a single go
        org-refile-use-outline-path t)         ; Show full paths for refiling

  ;; Refile between files
  ;; (for some reason I had to put this setting
  ;; here, by itself, for it to work)
  (setq org-refile-use-outline-path 'file)

  ;; Appearance
  (setq org-tags-column 0                                   ; Position tags next to heading
        org-startup-folded t
        org-blank-before-new-entry (quote ((heading . auto) ; Empty line before headings
                                           (plain-list-item . nil)))
        org-src-preserve-indentation t)                     ; Don't add unnecessary indentation

  ;; Make only first org heading be bold
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :weight normal))))
   '(org-level-3 ((t (:inherit outline-3 :weight normal))))
   '(org-level-4 ((t (:inherit outline-4 :weight normal))))
   '(org-level-5 ((t (:inherit outline-5 :weight normal))))
   '(org-level-6 ((t (:inherit outline-6 :weight normal))))
   '(org-level-7 ((t (:inherit outline-7 :weight normal))))
   '(org-level-8 ((t (:inherit outline-8 :weight normal))))))

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
        org-element-use-cache nil              ; org element cache often produced errors, so I disabled it
        org-agenda-scheduled-leaders '("" "")  ; Hide "Scheduled" text
        org-agenda-prefix-format "  %?-12t% s" ; Hide category for agenda items

        org-agenda-files '("~/notes/org")
        ;; Add newline above date heading
        org-agenda-format-date
        (lambda (date)
          (concat "\n" (org-agenda-format-date-aligned date)))

        ;; Time grid settings
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "...." "------------")
        org-agenda-current-time-string
        "← now")

  (setq org-agenda-custom-commands
        '(("w" "Week agenda"
           ((agenda "" ((org-agenda-span 'week)))))))

  ;; Date heading settings
  (custom-set-faces
   '(org-agenda-date ((t (:height 1.0 :weight bold :background "unspecified"))))
   '(org-agenda-date-today ((t (:height 1.3 :weight bold :background "unspecified" :underline nil))))))

(use-package org-capture
  :ensure nil
  :after org
  :config
  ;; Don't save org capture bookmarks
  (setq org-bookmark-names-plist nil
        org-capture-bookmark nil)
  :custom
  (org-capture-templates
   '(
     ("t" "Task" entry (file "")
      "* TODO %?\n  %i\n")

     ("l" "Task with link" entry (file "")
      "* TODO %?\n  %i\n %a")

     ("n" "Note" entry (file "")
      "* %?\n %i\n")

     ("e" "Calendar event" entry (file "calendar.org")
      "* %?\n %^t")

     ("c" "Contact" entry (file "")
      "* %?
          :PROPERTIES:
          :PHONE: %^{phone number}
          :ADDRESS: %^{Street name Street no., Postal Code Postal Area, Country}
          :BIRTHDAY: %^{yyyy-mm-dd}
          :EMAIL: %^{name@domain.com}
          :NOTE: %^{NOTE}
          :END:"))))

(use-package org-contacts
  :after org
  :pin melpa
  :defer t
  :custom (org-contacts-files '("~/notes/org/contacts.org")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (java . t)))

(setq org-confirm-babel-evaluate nil)
(org-babel-tangle-file "~/.emacs.d/init.org")

;; Block templates
(setq org-structure-template-alist
      '(("l" . "src emacs-lisp")
        ("j" . "src java")
        ("s" . "src")
        ("e" . "example")
        ("q" . "quote")))

;; Calendar framework
(use-package calfw
  :config
  ;; Use Swedish calendar
  (load "sv-kalender"))

;; Integrate calfw with org
(use-package calfw-org
  :after calfw)

(defun my/custom-open-calendar ()
  "Open calendar with two weeks view"
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'two-weeks))

(provide 'm-org)
