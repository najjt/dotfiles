;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(provide 'm-pdf)
