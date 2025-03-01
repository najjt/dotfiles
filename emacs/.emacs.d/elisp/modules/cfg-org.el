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
        org-todo-keywords '((sequence "TODO" "|" "DONE"))
        org-refile-targets (quote (("personal.org"            :maxlevel   . 2)
                                   ("backlog.org"             :maxlevel   . 2)
                                   ("repeating.org"           :maxlevel   . 2)
                                   ("calendar.org"            :maxlevel   . 1)
                                   ("../misc/computer.org"    :maxlevel   . 1))))

  ;; Refile settings
  (setq org-refile-use-outline-path 'file       ; Provide refile targets as path
        org-outline-path-complete-in-steps nil) ; Refile in a single go

  ;; Appearance
  (setq org-tags-column 0               ; Position tags next to heading
        org-startup-folded t
        org-src-preserve-indentation t  ; Don't add unnecessary indentation
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

  ;; Activate the habit module for org mode
  (add-to-list 'org-modules 'org-habit t)

  (defvar my/org-habit-show-graphs-everywhere t
    "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")

  (defun my/org-agenda-mark-habits ()
    "Mark all habits in current agenda for graph display.

This function enforces `my/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `my/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
    (when (and my/org-habit-show-graphs-everywhere
               (not (get-text-property (point) 'org-series)))
      (let ((cursor (point))
            item data)
        (while (setq cursor (next-single-property-change cursor 'org-marker))
          (setq item (get-text-property cursor 'org-marker))
          (when (and item (org-is-habit-p item))
            (with-current-buffer (marker-buffer item)
              (setq data (org-habit-parse-todo item)))
            (put-text-property cursor
                               (next-single-property-change cursor 'org-marker)
                               'org-habit-p data))))))

  (advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits))

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
        ;; org element cache often produced errors, so I disabled it
        org-element-use-cache nil
        org-agenda-files '("~/notes/org")

        ;; Time grid settings
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "...." "------------")
        org-agenda-current-time-string
        "← now")

  (setq org-agenda-custom-commands
        '(("w" "Week agenda"
           ((agenda "" ((org-agenda-span 'week)))))
          ("h" "Habits" tags-todo "STYLE=\"habit\"" ((org-agenda-overriding-header "Habits")))))

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
  :custom (org-contacts-files '("~/notes/org/contacts.org")))

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

;; Calendar framework
(use-package calfw
  :config
  ;; Use Swedish calendar
  (load "sv-kalender"))

;; Integrate calfw with org
(use-package calfw-org
  :after calfw
  :config
  ;; Hotfix: incorrect time range display
  ;; source: https://github.com/zemaye/emacs-calfw/commit/3d17649c545423d919fd3bb9de2efe6dfff210fe
  (defun cfw:org-get-timerange (text)
    "Return a range object (begin end text).
If TEXT does not have a range, return nil."
    (let* ((dotime (cfw:org-tp text 'dotime)))
      (and (stringp dotime) (string-match org-ts-regexp dotime)
           (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
                  (start-date (nth 1 (car matches)))
                  (end-date (nth 1 (nth 1 matches)))
                  (extra (cfw:org-tp text 'extra)))
             (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
                 ( list( calendar-gregorian-from-absolute
                         (time-to-days
                          (org-read-date nil t start-date))
                         )
                   (calendar-gregorian-from-absolute
                    (time-to-days
                     (org-read-date nil t end-date))) text)))))))

(defun my/custom-open-calendar ()
  "Open calendar with two weeks view"
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'two-weeks))

;; Open calendar
(keymap-global-set "C-c k" 'my/custom-open-calendar)

;; Generate mind maps from org files
(use-package org-mind-map
  :init
  (require 'ox-org))

;; View statistics for habits
(use-package org-habit-stats
  :bind ((:map org-mode-map
               ("C-c h" . org-habit-stats-view-habit-at-point))
         (:map org-agenda-mode-map
               ("C-c h" . org-habit-stats-view-habit-at-point-agenda))))

(provide 'cfg-org)
