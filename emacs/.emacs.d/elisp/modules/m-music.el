;; -*- lexical-binding: t; -*-

(use-package emms
  :bind (("C-c e e" . emms)
         ("C-c e b" . emms-browser)
         :map emms-browser-mode-map
         ("C-a" . emms-browser-add-tracks-and-play))
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native)
        emms-source-file-default-directory "~/Music/"))

(provide 'm-music)
