;; -*- lexical-binding: t; -*-

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-items '((recents   . 5)
                    (bookmarks . 5))
        dashboard-startup-banner "~/.emacs.d/banner.txt"
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items)
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons nil
        dashboard-set-file-icons t))

(provide 'm-dashboard)
