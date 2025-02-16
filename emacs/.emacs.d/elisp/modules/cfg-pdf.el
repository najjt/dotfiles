;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("M-n" . pdf-view-next-page)
              ("M-p" . pdf-view-previous-page))
  :config
  (pdf-tools-install))

(provide 'cfg-pdf)
