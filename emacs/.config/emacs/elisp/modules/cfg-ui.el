;; -*- lexical-binding: t; -*-

;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))

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

;; Set font
(let ((mono-spaced-font "monospace")
      (proportionately-spaced-font "sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 115)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.15)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.1))

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
             flymake-mode
             hs-minor-mode))

(with-eval-after-load 'subword
  (diminish 'subword-mode))

;; Show full path to file in header line
(defun my/set-header-line-for-files-only ()
  "Set `header-line-format` to show bold file name info only in file-visiting buffers."
  (setq header-line-format
        (when buffer-file-name
          (let ((file-name (abbreviate-file-name buffer-file-name)))
            (concat (propertize file-name 'face 'italic))))))

(add-hook 'after-change-major-mode-hook #'my/set-header-line-for-files-only)

;; Visualize color codes in text
(use-package colorful-mode
  :diminish
  :config
  (global-colorful-mode t))

(defun my/select-window (window &rest _)
  "Select WINDOW for display-buffer-alist"
  (select-window window))

;; Settings for displaying buffers
(setq display-buffer-alist
      '(
        ((or . ((derived-mode . occur-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (body-function . my/select-window)
         (dedicated . t)
         (preserve-size . (t . t)))

        ((or . ((derived-mode . compilation-mode)))
         (display-buffer-reuse-window display-buffer-same-window)
         (body-function . my/select-window)
         (dedicated . t)
         (inhibit-same-window . nil)
         (preserve-size . (t . t)))

        ((or . ((derived-mode . helpful-mode)))
         (display-buffer-reuse-mode-window)
         (dedicated . t))

        ;; Hide compilation windows
        ("\\*compilation\\*" display-buffer-no-window
         (allow-no-window . t))))

(use-package nerd-icons-corfu
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Enable context menu (mouse-click menu)
(dolist (hook '(text-mode-hook
                prog-mode-hook
                dired-mode-hook))
  (add-hook hook 'context-menu-mode))

;; Consider all themes as safe
(setq custom-safe-themes t)

(provide 'cfg-ui)
