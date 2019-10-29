(use-package helm
  :bind ("M-x" . 'helm-M-x)
  :config
  (evil-leader/set-key
    "bb" 'helm-mini
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "hi" 'helm-imenu
    "hr" 'helm-recentf
    "hk" 'helm-show-kill-ring
    ))

(provide 'init-helm)
