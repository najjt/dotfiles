;; -*- lexical-binding: t; -*-

;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Show column number in status bar
(column-number-mode)

;; Show fringes
(fringe-mode 8)

;; Hide tab bar if only one tab
(setq tab-bar-show 1)

;; Make line numbers relative
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; Display line numbers in the below modes
(dolist (hook '(conf-mode-hook
                prog-mode-hook
                text-mode-hook
                markdown-mode-hook
                org-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;; Highlight current line
(dolist (hook '(text-mode-hook
                prog-mode-hook
                dired-mode-hook
                Man-mode-hook
                conf-mode-hook
                emms-browser-mode-hook
                emms-playlist-mode-hook
                org-agenda-mode-hook
                Info-mode-hook
                messages-buffer-mode-hook
                mu4e-view-mode-hook
                elfeed-show-mode-hook))
  (add-hook hook 'hl-line-mode))

;; Make keybindings in minibuffer look like other text
(set-face-attribute 'help-key-binding nil
                    :box nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit nil)

;; Set font
(let ((mono-spaced-font "Liberation Mono")
      (proportionately-spaced-font "Libertinus Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 120)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.2)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.2))

(defun my/disable-all-themes ()
  "Disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/enable-theme (theme)
  "Interactively enable the specified THEME and disable all other themes."
  (interactive
   (list (completing-read "Choose theme: " (mapcar #'symbol-name (custom-available-themes)))))
  (my/disable-all-themes)
  (load-theme (intern theme) t)
  (customize-save-variable 'my-chosen-theme theme))

;; Remember last used theme between sessions
(add-hook 'after-init-hook
          (lambda ()
            (if (boundp 'my-chosen-theme)
                (my/enable-theme my-chosen-theme))))

;; Hide minor modes in modeline
(use-package diminish
  :diminish (auto-fill-function
             centered-window-mode
             eldoc-mode
             evil-collection-unimpaired-mode
             org-indent-mode
             abbrev-mode
             subword-mode
             flymake-mode
             hs-minor-mode))

;; Center buffers
(use-package olivetti
  :defer t
  :commands (olivetti-mode)
  :custom (olivetti-body-width 85)
  :init
  (defun my/olivetti-conditional-enable ()
    "Enable `olivetti-mode` only in Org or Markdown buffers."
    (when (derived-mode-p 'org-mode 'markdown-mode)
      (olivetti-mode 1)))

  (defun my/olivetti-conditional-disable ()
    "Disable `olivetti-mode` if not in Org or Markdown buffers."
    (unless (derived-mode-p 'org-mode 'markdown-mode)
      (when (bound-and-true-p olivetti-mode)
        (olivetti-mode -1))))

  ;; Add hooks globally at init time
  (add-hook 'after-change-major-mode-hook #'my/olivetti-conditional-disable)
  (add-hook 'org-mode-hook #'my/olivetti-conditional-enable)
  (add-hook 'markdown-mode-hook #'my/olivetti-conditional-enable))

;; Show full path to file in header line
(defun my/set-header-line-for-files-only ()
  "Set `header-line-format` to show bold file name info only in file-visiting buffers."
  (setq header-line-format
        (when buffer-file-name
          (let ((file-name (abbreviate-file-name buffer-file-name)))
            (concat (propertize file-name 'face 'bold))))))

(add-hook 'after-change-major-mode-hook #'my/set-header-line-for-files-only)

;; Ensure tabs use fixed pitch font
(custom-set-faces
 '(tab-bar ((t (:inherit fixed-pitch :height 0.8)))))

;; Visualize color codes in text
(use-package colorful-mode
  :diminish
  :custom
  (colorful-use-prefix nil)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t))

(defun my/select-window (window &rest _)
  "Select WINDOW for display-buffer-alist"
  (select-window window))

;; Settings for displaying buffers
(setq display-buffer-alist
      '(((or . ((derived-mode . occur-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (body-function . my/select-window)
         (dedicated . t)
         (preserve-size . (t . t)))

        ("\\*grep\\*"
         (display-buffer-reuse-mode-window)
         (body-function . my/select-window)
         (dedicated . t)
         (preserve-size . (t . t)))))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (when (display-graphic-p)
                                       (nerd-icons-completion-mode)))))

(use-package nerd-icons-corfu
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'cfg-ui)
