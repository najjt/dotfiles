;; -*- lexical-binding: t; -*-

(use-package emms
  :bind (("C-c e"   . emms-browser)
         ("C-c C-x e" . emms-playlist-mode-go-popup)
         :map emms-browser-mode-map
         ("<normal-state> q" . emms-filters-pop-cache)
         ("q" . emms-filters-pop-cache))
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native)
        emms-source-file-default-directory "~/Music/"
        emms-browser-covers 'emms-browser-cache-thumbnail-async
        emms-playlist-mode-window-width 80
        emms-mode-line-format "")

  (defun emms-get-album-art (track-path)
    "Get the album art for the track located at TRACK-PATH.
Returns the path to the album art or a default image if no album art is found."
    (let* ((album-dir (file-name-directory track-path))
           (album-art (or (and (file-exists-p (concat album-dir "cover.jpg"))
                               (concat album-dir "cover.jpg"))
                          (and (file-exists-p (concat album-dir "folder.jpg"))
                               (concat album-dir "folder.jpg"))
                          (and (file-exists-p (concat album-dir "cover.png"))
                               (concat album-dir "cover.png"))
                          nil)))
      album-art))

  (defun emms-notify-track-description ()
    "Use `notify-send' to show the description of the current track with album art."
    (interactive)
    (let* ((track (emms-playlist-current-selected-track))
           (title (emms-track-get track 'info-title))
           (artist (emms-track-get track 'info-albumartist))
           (album (emms-track-get track 'info-album))
           (track-path (emms-track-get track 'name))
           (album-art (emms-get-album-art track-path)))

      ;; Limit the title to 40 characters
      (setq title (if (> (length title) 40)
                      (concat (substring title 0 37) "...")  ; Truncate and add "..."
                    title))

      ;; Limit the artist name to 40 characters
      (setq artist (if (> (length artist) 40)
                       (concat (substring artist 0 37) "...")
                     artist))

      ;; Limit the album title to 40 characters
      (setq album (if (> (length album) 40)
                      (concat (substring album 0 37) "...")
                    album))

      ;; Send the notification
      (call-process "notify-send"
                    nil nil nil
                    "-i" album-art
                    (format "🎝 %s" title)
                    (format "%s\n<i>%s</i>" artist album))))

  (add-hook 'emms-player-started-hook #'emms-notify-track-description))

(provide 'cfg-music)
