;; -*- lexical-binding: t; -*-

(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified
                  mode-line-remote)
                 display (min-width (5.0)))
                mode-line-frame-identification mode-line-buffer-identification "   "
                mode-line-position (vc-mode vc-mode) "  " my-modeline-major-mode
                mode-line-misc-info mode-line-end-spaces))

;; Source: https://protesilaos.com/codelog/2023-07-29-emacs-custom-modeline-tutorial/
(defun my-modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))

(defvar-local my-modeline-major-mode
    '(:eval
      (list
       (propertize "λ" 'face 'shadow)
       " "
       (propertize (my-modeline--major-mode-name) 'face 'bold)))
  "Mode line construct to display the major mode.")

(put 'my-modeline-major-mode 'risky-local-variable t)

;; Hide minor modes in modeline
(use-package diminish
  :diminish (auto-fill-function
             centered-window-mode
             eldoc-mode
             evil-collection-unimpaired-mode
             org-indent-mode
             abbrev-mode
             subword-mode
             flymake-mode))

;; Disable border around modelines
;; and use variable pitch font
(custom-set-faces
 '(mode-line ((t (:box nil :inherit variable-pitch))))
 '(mode-line-inactive ((t (:box nil :inherit variable-pitch)))))

(provide 'cfg-modeline)
