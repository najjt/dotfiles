;; -*- lexical-binding: t; -*-

(setq TeX-source-correlate-method 'synctex)

(use-package auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-mode 1)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(provide 'cfg-tex)
