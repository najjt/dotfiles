;; -*- lexical-binding: t; -*-

(use-package emms
  :defer t
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
        emms-mode-line-format "")

  ;; Function to send a notification to dunst with the song details and album art
  (defun emms-show-notification ()
    "Send a notification with the current track, artist, album name, and cover art."
    (start-process-shell-command "notify-song" nil "$HOME/scripts/notify_song.sh"))

  ;; Hook into the track change event to trigger the notification
  (add-hook 'emms-player-started-hook #'emms-show-notification))

(provide 'cfg-music)
