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
  ;; Refile settings
  (setq org-default-notes-file (concat org-directory "/refile.org")
        org-refile-targets (quote (("todo.org"                :maxlevel   . 2)
                                   ("stash.org"               :maxlevel   . 2)
                                   ("repeating.org"           :maxlevel   . 2)
                                   ("calendar.org"            :level      . 0)
                                   ("../misc/dator.org"    :maxlevel   . 1)))
        org-refile-use-outline-path 'file       ; Provide refile targets as path
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
        ;; org element cache often produced errors, so I disabled it
        org-element-use-cache nil
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
          ("p" "Past events"
           ((tags "TIMESTAMP<=\"<now>\""))
           ((org-agenda-files
             (cl-remove-if
              (lambda (file)
                (string-match-p "repeating\\.org$" file))
              (org-agenda-files)))))))

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
                     (org-read-date nil t end-date))) text))))))

  ;; Hotfixes for ensuring that string-match does not get nil input
  ;; Source: https://github.com/kiwanami/emacs-calfw/pull/155
  (defun cfw:org-summary-format (item)
    "Format an item. (How should be displayed?)"
    (let* ((time (cfw:org-tp item 'time))
           (time-of-day (cfw:org-tp item 'time-of-day))
           (time-str (and time-of-day
                          (format "%02i:%02i " (/ time-of-day 100) (% time-of-day 100))))
           (category (cfw:org-tp item 'org-category))
           (tags (cfw:org-tp item 'tags))
           (marker (cfw:org-tp item 'org-marker))
           (buffer (and marker (marker-buffer marker)))
           (text (cfw:org-extract-summary item))
           (props (cfw:extract-text-props item 'face 'keymap))
           (extra (cfw:org-tp item 'extra)))
      (setq text (substring-no-properties text))
      (when (and extra (string-match (concat "^" org-deadline-string ".*") extra))
        (add-text-properties 0 (length text) (list 'face (org-agenda-deadline-face 1.0)) text))
      (if org-todo-keywords-for-agenda
          (when (string-match (concat "^[\t ]*\\<\\(" (mapconcat 'identity org-todo-keywords-for-agenda "\\|") "\\)\\>") text)
            (add-text-properties (match-beginning 1) (match-end 1) (list 'face (org-get-todo-face (match-string 1 text))) text)))
    ;;; ------------------------------------------------------------------------
    ;;; act for org link
    ;;; ------------------------------------------------------------------------
      (setq text (replace-regexp-in-string "%[0-9A-F]\\{2\\}" " " text))
      (if (string-match org-bracket-link-regexp text)
          (let* ((desc (if (match-end 3) (org-match-string-no-properties 3 text)))
                 (link (org-link-unescape (org-match-string-no-properties 1 text)))
                 (help (concat "LINK: " link))
                 (link-props (list
                              'face 'org-link
                              'mouse-face 'highlight
                              'help-echo help
                              'org-link link)))
            (if desc
                (progn
                  (setq desc (apply 'propertize desc link-props))
                  (setq text (replace-match desc nil nil text)))
              (setq link (apply 'propertize link link-props))
              (setq text (replace-match link nil nil text)))))
      (when time-str
        (setq text (concat time-str text)))
      (propertize
       (apply 'propertize text props)
       ;; include org filename
       ;; (and buffer (concat " " (buffer-name buffer)))
       'keymap cfw:org-text-keymap
       ;; Delete the display property, since displaying images will break our
       ;; table layout.
       'display nil)))

  (defun cfw:org-get-timerange (text)
    "Return a range object (begin end text).
If TEXT does not have a range, return nil."
    (let* ((dotime (cfw:org-tp text 'dotime)))
      (and (stringp dotime) (and dotime (string-match org-ts-regexp dotime))
           (let ((date-string  (match-string 1 dotime))
                 (extra (cfw:org-tp text 'extra)))
             (if (and extra (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra))
                 (let* ((cur-day (string-to-number
                                  (match-string 1 extra)))
                        (total-days (string-to-number
                                     (match-string 2 extra)))
                        (start-date (time-subtract
                                     (org-read-date nil t date-string)
                                     (seconds-to-time (* 3600 24 (- cur-day 1)))))
                        (end-date (time-add
                                   (org-read-date nil t date-string)
                                   (seconds-to-time (* 3600 24 (- total-days cur-day))))))
                   (list (calendar-gregorian-from-absolute (time-to-days start-date))
                         (calendar-gregorian-from-absolute (time-to-days end-date)) text))
               )))))
  )

(defun my/custom-open-calendar ()
  "Open calendar with two weeks view"
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'two-weeks))

;; Generate mind maps from org files
(use-package org-mind-map
  :init
  (require 'ox-org))

(provide 'cfg-org)
