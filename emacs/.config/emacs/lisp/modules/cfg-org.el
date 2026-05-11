;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :bind (("C-c a"     . org-agenda)
	 ("C-c c"     . org-capture)
	 ("C-c l"     . org-store-link)
	 ("C-c M-RET" . org-insert-todo-heading)) ; For terminal
  :config
  (setq org-directory "~/notes/org"
	org-todo-keywords '((sequence "TODO" "|" "DONE"))
	org-M-RET-may-split-line '((default . nil))
	org-insert-heading-respect-content t
	org-log-done 'time
	org-log-into-drawer t)

  (load "sv-kalender")

  ;; Refile settings
  (setq org-default-notes-file (concat org-directory "/omarkiv.org")
	org-refile-targets (quote (("todo.org"		:maxlevel . 2)
				   ("rep.org"		:maxlevel . 1)
				   ("arkiv.org"		:maxlevel . 1)
				   ("varia.org"		:maxlevel . 1)
				   ("kalender.org"	:level    . 0)))
	org-refile-use-outline-path 'file       ; Provide refile targets as path
	org-outline-path-complete-in-steps nil) ; Refile in a single go

  ;; Appearance
  (setq org-tags-column 0               ; Position tags next to heading
	org-startup-folded t
	org-src-preserve-indentation t  ; Don't add unnecessary indentation
	;; Hide file names in agenda buffer
	org-agenda-prefix-format
	'((agenda . " %i %?-12t% s")
	  (todo   . " %i %-12:c")
	  (tags   . " %i %-12:c")
	  (search . " %i %-12:c"))
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
   '(org-level-8 ((t (:inherit outline-8 :weight normal))))))

;; Generate a table of contents
(use-package toc-org
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
	   ((agenda "" ((org-agenda-span 'week)))))
	  ("k" "Month agenda"
	   ((agenda "" ((org-agenda-span 'month)))))))

  ;; Date heading settings
  (custom-set-faces
   '(org-agenda-date ((t (:height 1.0 :weight bold :background unspecified))))
   '(org-agenda-date-today ((t (:height 1.3 :weight bold :background unspecified :underline unspecified))))))

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

     ("e" "Calendar event" entry (file "kalender.org")
      "* %?\n%^t")

     ("c" "Contact" entry (file "kontakter.org")
      "* %? \n:PROPERTIES:\n:PHONE: %^{Phone number}\n:END:"))))

(use-package org-contacts
  :after org
  :pin melpa
  :custom (org-contacts-files '("~/notes/org/kontakter.org")))

(use-package org-wild-notifier
  :config
  (org-wild-notifier-mode)
  (setq org-wild-notifier-day-wide-alert-times '("07:00")))

(provide 'cfg-org)
