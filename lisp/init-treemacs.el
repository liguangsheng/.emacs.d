(use-package treemacs
  :config
  (treemacs-resize-icons 12)
  (defun treemacs-switch-window ()
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
	(aw-flip-window)
      (progn
      (aw--push-window (selected-window))
      (treemacs-select-window))))

  (evil-leader/set-key
    "tw" 'treemacs
    "tt" 'treemacs-switch-window
    "-"  'treemacs-switch-window
    ))

(use-package treemacs-projectile)

(use-package treemacs-evil)

(provide 'init-treemacs)
