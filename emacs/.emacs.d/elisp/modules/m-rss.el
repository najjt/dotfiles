;; -*- lexical-binding: t; -*-

(use-package elfeed
  :bind ("C-c w" . elfeed)
  :config
  (setq elfeed-feeds
        '("https://protesilaos.com/commentary.xml"
          ;; Dax Flame
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCG2JqxPd0Yd_XPK9shr-J_w"
          ;; Gavin Freeborn
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w"
          ;; Protesilaos Stavrou
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g")))

(provide 'm-rss)
