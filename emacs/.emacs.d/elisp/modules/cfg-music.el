;; -*- lexical-binding: t; -*-

;; MPD client
(use-package mpdel
  :ensure t
  :diminish
  :init (setq mpdel-prefix-key (kbd "C-c z"))
  :config
  (mpdel-mode)

  ;; Turn off evil mode for mpdel
  (evil-set-initial-state 'mpdel-browser-mode 'emacs)
  (evil-set-initial-state 'mpdel-playlist-mode 'emacs)
  (evil-set-initial-state 'mpdel-playlist-current-playlist-mode 'emacs)
  (evil-set-initial-state 'mpdel-playlist-stored-playlist-mode 'emacs)
  (evil-set-initial-state 'mpdel-song-mode 'emacs)
  (evil-set-initial-state 'mpdel-tablist-mode 'emacs)

  ;; Key bindings for mpdel browser and playlist modes
  (bind-key "j" 'tablist-next-line mpdel-browser-mode-map)
  (bind-key "k" 'tablist-previous-line mpdel-browser-mode-map)

  (bind-key "j" 'tablist-next-line mpdel-playlist-mode-map)
  (bind-key "k" 'tablist-previous-line mpdel-playlist-mode-map)

  (bind-key "j" 'tablist-next-line mpdel-playlist-current-playlist-mode-map)
  (bind-key "k" 'tablist-previous-line mpdel-playlist-current-playlist-mode-map)

  (bind-key "j" 'tablist-next-line mpdel-playlist-stored-playlist-mode-map)
  (bind-key "k" 'tablist-previous-line mpdel-playlist-stored-playlist-mode-map)

  (bind-key "j" 'tablist-next-line mpdel-tablist-mode-map)
  (bind-key "k" 'tablist-previous-line mpdel-tablist-mode-map))

(provide 'cfg-music)
