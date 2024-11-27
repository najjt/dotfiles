;; -*- lexical-binding: t; -*-

(use-package elfeed
  :bind ("C-c w" . (lambda ()
                     (interactive)
                     (elfeed)
                     (elfeed-update)))
  :config
  (setq elfeed-feeds
        '(;; Prot's commentary on life
          "https://protesilaos.com/commentary.xml"
          ;; Prot's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g"
          ;; Dax Flame's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCG2JqxPd0Yd_XPK9shr-J_w"
          ;; Gavin Freeborn's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w"
          ;; Bread on Penguins's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCwHwDuNd9lCdA7chyyquDXw")))

(provide 'm-rss)
