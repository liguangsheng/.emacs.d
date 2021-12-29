(use-package emojify
  :init
  (setq emojify-display-style 'image)
  :hook (after-init . global-emojify-mode))

(provide 'init-emoji)
