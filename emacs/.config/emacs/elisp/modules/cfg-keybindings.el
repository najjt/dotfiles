;; -*- lexical-binding: t; -*-

;; Keybindings

;; Additional keybinding for M-x
(keymap-global-set "C-c k" 'execute-extended-command)

;; Increase/decrease text scale
(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "C-+" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)

;; Make escape quit prompts
(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; Keybind workarounds for terminal Emacs
(define-key key-translation-map (kbd "C-x ,") (kbd "C-x C-;")) ; comment-line
(define-key key-translation-map (kbd "C-c ,") (kbd "C-c C-,")) ; org-insert-structure-template

(defun my/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(keymap-global-set "S-<return>" #'my/smart-open-line)

(keymap-global-set "M-g r" 'recentf)

(keymap-global-set "M-s g" 'grep)
(keymap-global-set "M-s r" 'rgrep)
(keymap-global-set "M-s f" 'find-name-dired)

;; Move forward to beginning of next word
(keymap-global-set "M-F" 'forward-to-word)
;; Move backward to end of previous word
(keymap-global-set "M-B" 'backward-to-word)

(provide 'cfg-keys)
