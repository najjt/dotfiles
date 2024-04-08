(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "67f6b0de6f60890db4c799b50c0670545c4234f179f03e757db5d95e99bac332" "a53c7ff4570e23d7c5833cd342c461684aa55ddba09b7788d6ae70e7645c12b4" "f23398a9a8d9476cd5ffdd04f55159bd0c454a9d9ba3de145d637c42c3becddd" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "c7f838704d7caa88bc337464867c22af0a502e32154558b0f6c9c3c6e8650122" "aeb5508a548f1716142142095013b6317de5869418c91b16d75ce4043c35eb2b" "0794b33f9fb2afa48bcc09c4fb4f87d3f781b9f5847ccd47a78b8e3ca54b80d4" "f1b2de4bc88d1120782b0417fe97f97cc9ac7c5798282087d4d1d9290e3193bb" "18624b2da7749af193a4eeaa7be1dc2abe94a97a8562ba69f5ee0f06d6dd156e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "1bad38d6e4e7b2e6a59aef82e27639e7a1d8e8b06bbeac6730f3e492d4f5ba46" "10fef6d73ae453f39c9f325915386d41894870b72926e8e9b0c39d030447b703" "aa742450bc84284415b398be20bfe1c7e63b58fbbc4beb4f2709ce08f2ca3c92" "3074fda75f35f990d112fb75681729a74b6c7f15d3e5dfcf80313abb4cd39ed8" "38c4fb6c8b2625f6307f3dde763d5c61d774d854ecee9c5eb9c5433350bc0bef" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" "9b65cf71fd6b27a5362afeff062c6abd1c5d8a7c4d444c942f3da36bf0a151b1" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "9c22d1654fbfed0285a519541b5ea76462d9b03d778d1dc34779fd38613bf7c8" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "c3957b559cf3606c9a40777c5712671db3c7538e5d5ea9f63eb0729afeac832b" "02fefdfc9a0c7256a10c8794a4985c9c70c5fbf674873b66807e8143e02c81a7" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "c44056a7a2426e1229749e82508099ba427dde80290da16a409d5d692718aa11" "9cda05ef581a03ce47f9e490c241f659520be02a7318757048db32fef4421da9" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(my-chosen-theme 'modus-vivendi t)
 '(org-agenda-files
   '("/Users/najjt/Documents/notes/org/capture.org" "/Users/najjt/Documents/notes/org/privat.org" "/Users/najjt/Documents/notes/org/studier.org" "/Users/najjt/Documents/notes/org/computer.org" "/Users/najjt/Documents/notes/org/calendar.org"))
 '(package-selected-packages
   '(eglot-java lsp-java toc-org org-bullets perfect-margin writeroom perspective darkroom frame logos standard-themes org-contrib evil-magit magit counsel-projectile projectile request org-super-agenda flycheck-pos-tip pdf-tools plantuml-mode auctex flycheck-popup-tip flycheck-posframe flycheck auto-package-update modus-themes zen-mode undo-tree dashboard noflet mood-line vterm evil-nerd-commenter company-box company treemacs-nerd-icons hydra undo-fu doom-themes org-habit nerd-icons-dired org-superstar-mode ivy-prescient prescient popper rainbow-delimiters general exec-path-from-shell helpful ivy-rich org-archive markdown-mode counsel swiper ivy calfw-org calfw org-contacts diary evil-surround org-web-tools sourcerer which-key evil-collection diminish))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (if
                 (y-or-n-p "Tangle?")
                 (org-babel-tangle)))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (if
                 (y-or-n-p "Reload?")
                 (load-file user-init-file)))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:box nil))))
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil))))
 '(org-agenda-date ((t (:height 1.0 :weight bold))))
 '(org-agenda-date-today ((t (:height 1.0 :weight bold))))
 '(org-habit-alert-face ((t (:background "#dc322f"))))
 '(org-habit-alert-future-face ((t (:background "#cb4b16"))))
 '(org-habit-clear-face ((t (:background "#859900"))))
 '(org-habit-clear-future-face ((t (:j background "#586e75"))))
 '(org-habit-overdue-face ((t (:background "#d33682"))))
 '(org-habit-overdue-future-face ((t (:background "#6c71c4"))))
 '(org-habit-ready-face ((t (:background "#b58900"))))
 '(org-habit-ready-future-face ((t (:background "#657b83"))))
 '(org-habit-today-face ((t (:background "#268bd2")))))
