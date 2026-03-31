;; -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")

;; Automatic line breaking
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Spell checking
(use-package jinx
  :diminish
  :hook (((markdown-mode org-mode text-mode) . jinx-mode))
  :bind ("C-c s" . jinx-correct)
  :config
  (setq jinx-languages "sv en_US"))

;; Undo functionality
(use-package vundo
  :defer t)

;; Tabs are four spaces
(setq-default tab-width 4
              indent-tabs-mode nil)

;; Copy to system clipboard in terminal
(use-package clipetty
  :diminish
  :if (not (display-graphic-p))
  :hook (after-init . global-clipetty-mode))

(defun my/find-link-at-point()
  "Returns the link at point. Improved version of the `browse-url-at-point'."
  (or (thing-at-point 'url t)
      (get-text-property (point) 'shr-url)
      (and (derived-mode-p 'org-mode)
           (org-element-property :raw-link (org-element-context)))))

(defun my/browse-url-at-point ()
  "Browse the URL at point with special cases handling."
  (interactive)
  (unless (display-graphic-p)
    (when-let* ((url (my/find-link-at-point))
                ((string-prefix-p "http" url)))
      (browse-url url)
      t)))

;; Use org-mode keybind for opening links everywhere
(keymap-global-set "C-c C-o" #'my/browse-url-at-point)

;; Have `org-open-at-point' to try `my/browse-url-at-point' before
;; built-in function.
(with-eval-after-load "org"
  (add-hook 'org-open-at-point-functions #'my/browse-url-at-point))

;; Writable grep buffers
(use-package wgrep)

(provide 'cfg-text)
