(use-package projectile)

(use-package helm-projectile
  :config
  (evil-leader/set-key
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file-dwim
    "pg" 'helm-projectile-grep
    "pp" 'helm-projectile
    "pr" 'helm-projectile-recentf
    "ps" 'projectile-run-eshell
    ))

(provide 'init-projectile)
