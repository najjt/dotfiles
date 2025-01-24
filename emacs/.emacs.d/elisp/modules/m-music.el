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
        emms-playlist-mode-window-width 75
        emms-mode-line-format ""))

(provide 'm-music)
