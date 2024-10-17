(use-package general
  :config

  ;; Open Hydra main menu
  (general-define-key
   :keymaps '(normal visual emacs)
   "," 'hydra-main/body)

  ;; Make <escape> quit prompts
  (general-define-key
   "<escape>" 'keyboard-escape-quit)

  ;; Increase/decrease text size
  (general-define-key
   "C-=" #'text-scale-increase
   "C-+" #'text-scale-increase
   "C--" #'text-scale-decrease))

(provide 'ml-keys)
