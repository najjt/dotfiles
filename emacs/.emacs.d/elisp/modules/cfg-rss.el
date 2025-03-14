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
          ;; Bread on Penguins's youtube channel
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCwHwDuNd9lCdA7chyyquDXw"
          "https://www.debian.org/News/news"
          )))

(provide 'cfg-rss)
