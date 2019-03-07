(use-package dashboard
  :config
  (defun dashboard-insert-profile (x)
    (insert "LGS-Emacs"))
  (add-to-list 'dashboard-item-generators  '(profile . dashboard-insert-profile))
  (setq 
	dashboard-banner-logo-title "♥Happy Hacking Emacs♥"
	dashboard-items '((recents . 20)
			  (projects . 10)
			  (agenda . 10)
			  (bookmarks . 10)
			  (registers . 5)))
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
