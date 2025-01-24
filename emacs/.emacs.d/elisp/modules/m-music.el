;; -*- lexical-binding: t; -*-

(use-package emms
  :bind (("C-c e e" . emms-smart-browse)
         ("C-c e p" . emms-playlist-mode-go-popup)
         :map emms-browser-mode-map
         ("C-a" . emms-browser-add-tracks-and-play))
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native)
        emms-source-file-default-directory "~/Music/"
        emms-browser-covers 'emms-browser-cache-thumbnail-async
        emms-playlist-mode-window-width 75))

;; Cycle currently playing track title and display time separately
(use-package emms-mode-line-cycle
  :init
  (emms-mode-line-cycle 1)
  :custom
  (emms-mode-line-cycle-velocity 3))

(provide 'm-music)
