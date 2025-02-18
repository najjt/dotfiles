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
  (defun emms-dunst-notify ()
    "Send a notification to dunst with the current track, artist, album name, and cover art."
    (let ((track (emms-playlist-current-selected-track))
          (title (emms-track-get (emms-playlist-current-selected-track) 'info-title))
          (artist (emms-track-get (emms-playlist-current-selected-track) 'info-artist))
          (album (emms-track-get (emms-playlist-current-selected-track) 'info-album)))

      (when track
        (start-process-shell-command
         "dunst-notify" nil
         (format "notify-send '%s' '%s, <i>%s</i>' -t 5000"
                 title artist album)))))

  ;; Hook into the track change event to trigger the notification
  (add-hook 'emms-player-started-hook 'emms-dunst-notify))

(provide 'cfg-music)
