;; Display right and left fringe
(fringe-mode '(8 . 8))

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Show column number in status bar
(column-number-mode)

;; Disable border around modelines
(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))

;; Make line numbers relative
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; Display relative line numbers in the below modes
(dolist (hook '(fundamental-mode conf-mode-hook prog-mode-hook text-mode-hook markdown-mode-hook org-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;; Highlight current line
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook dired-mode-hook Man-mode-hook conf-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Make keybindings in minibuffer look like other text
(set-face-attribute 'help-key-binding nil
                    :box nil
                    :foreground "unspecified"
                    :background "unspecified"
                    :inherit nil)

(when *sys/linux*
  (add-to-list 'default-frame-alist '(font . "Terminus (TTF)-11")))

(when *sys/mac*
  (add-to-list 'default-frame-alist '(font . "Iosevka-18")))

(use-package nerd-icons)

;; Use nerd icons in ibuffer
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package modus-themes
  :defer t)

(use-package ef-themes
  :defer t)

(provide 'ml-ui)
