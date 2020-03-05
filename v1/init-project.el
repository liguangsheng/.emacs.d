
(use-package projectile
  :init
  (setq projectile-known-projects-file (concat emacs-cache-dir "projectile-bookmarks.eld")
        projectile-cache-file          (concat emacs-cache-dir "projectile.cache")))

  "mm" 'er/mark-
(use-package helm-projectile
  :config
  (helm-projectile-on))

(provide 'init-project)
