(use-package projectile
  :commands projectile-project-root
  :bind (:map projectile-mode-map
	      ("s-t"   . projectile-find-file) ; `cmd-t' or `super-t'
	      ("C-c p" . projectile-command-map))

  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t
	projectile-cache-file (expand-dotlocal "projectile.cache")
	projectile-known-projects-file (expand-dotlocal "projectile-bookmark.eld"))

  :config
  (projectile-mode +1)
  ;; (when (and (not (executable-find "fd"))
  ;;            (executable-find "rg"))
  ;;   (setq projectile-generic-command
  ;;         (let ((rg-cmd ""))
  ;;           (dolist (dir projectile-globally-ignored-directories)
  ;; 			  (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
  ;;           (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  (when *windows*
    (setq projectile-indexing-method 'native
	  projectile-enable-caching  nil)

    ;; (when (or (executable-find "fd") (executable-find "rg"))
    ;;   (setq projectile-indexing-method 'alien
    ;;         projectile-enable-caching t))

    ;; (setq projectile-git-submodule-command nil)

    ;; Support Perforce project
    (let ((val (or (getenv "P4CONFIG") ".p4config")))
      (add-to-list 'projectile-project-root-files-bottom-up val)))
  )


(provide 'init-projectile)
