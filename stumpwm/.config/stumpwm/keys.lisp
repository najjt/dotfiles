;; -*-lisp-*-

;; Lock screen
(define-key *root-map* (kbd "C-l") "exec loginctl lock-session")

;; Buffer management
(define-key *root-map* (kbd "b") "windowlist")
(define-key *root-map* (kbd "d") "delete-window")
(define-key *root-map* (kbd "D") "delete-window-and-frame")
(define-key *root-map* (kbd "k") "kill-window")
(define-key *root-map* (kbd "n") "next")
(define-key *root-map* (kbd "o") "other-window")
(define-key *root-map* (kbd "p") "prev")

;; Frame management

;; Floating frames
(defvar *my-frames-float-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "float-this")
    (define-key m (kbd "F") "unfloat-this")
    (define-key m (kbd "u") "unfloat-this")
    (define-key m (kbd "C-f") "flatten-floats")
    m))

(defvar *my-frames-management-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "h") "move-focus left")
    (define-key m (kbd "j") "move-focus down")
    (define-key m (kbd "k") "move-focus up")
    (define-key m (kbd "l") "move-focus right")
    (define-key m (kbd "H") "move-window left")
    (define-key m (kbd "J") "move-window down")
    (define-key m (kbd "K") "move-window up")
    (define-key m (kbd "L") "move-window right")
    (define-key m (kbd "C-c") "exchange-direction left")
    (define-key m (kbd "C-t") "exchange-direction down")
    (define-key m (kbd "C-s") "exchange-direction up")
    (define-key m (kbd "C-r") "exchange-direction right")
    (define-key m (kbd "/") "hsplit-and-focus")
    (define-key m (kbd "-") "vsplit-and-focus")
    (define-key m (kbd "+") "balance-frames")
    (define-key m (kbd "d") "remove-split")
    (define-key m (kbd "D") "only")
    (define-key m (kbd "f") "fullscreen")
    (define-key m (kbd "F") '*my-frames-float-keymap*)
    (define-key m (kbd "i") "info")
    (define-key m (kbd "I") "show-window-properties")
    m))

(define-key *root-map* (kbd "w") '*my-frames-management-keymap*)

;; Media & brightness
(define-key *top-map* (kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map* (kbd "XF86AudioPrev") "exec playerctl previous")
(define-key *top-map* (kbd "XF86AudioNext") "exec playerctl next")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec ~/scripts/wm/buttons.sh volume_up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec ~/scripts/wm/buttons.sh volume_down")
(define-key *top-map* (kbd "XF86AudioMute") "exec ~/scripts/wm/buttons.sh volume_mute")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec ~/scripts/wm/buttons.sh brightness_down")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec ~/scripts/wm/buttons.sh brightness_up")

(which-key-mode)
