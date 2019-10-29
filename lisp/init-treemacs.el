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
    "-"  'treemacs-switch-window
    "="  'helm-treemacs-workspace
    "tr" 'treemacs
    "tt" 'treemacs-switch-window
    "tw" 'treemacs-switch-workspace
    ))

;;; helm-treemacs
(defun move-to-front (list x)
   (cons x (remove x list)))

(setq helm--treemacs-last-candidate "Default")

(defun helm--treemacs-workspace-candidates ()
  (move-to-front
   (cl-loop for ws in (treemacs-workspaces) collect (treemacs-workspace->name ws))
   helm--treemacs-last-candidate))


(defun treemacs-find-workspace (name)
  (seq-find
   (lambda (x) (string-equal name (treemacs-workspace->name x)))
   (treemacs-workspaces)))

(defun treemacs-select-workspace (ws)
  (setf (treemacs-current-workspace) ws)
  (treemacs--invalidate-buffer-project-cache)
  (treemacs--rerender-after-workspace-change)
  (run-hooks 'treemacs-switch-workspace-hook))

(defun treemacs-select-workspace-by-name (name)
  (treemacs-select-workspace (treemacs-find-workspace name))
  (message "treemacs select workspace: %s" name))

(defun helm-treemacs-workspace ()
  (interactive)
  (helm :sources (helm-build-sync-source "Helm-Treemacs"
		   :candidates (helm--treemacs-workspace-candidates)
		   :fuzzy-match t
		   :action (lambda (candidate)
			     (setq helm--treemacs-last-candidate (treemacs-workspace->name (treemacs-current-workspace)))
			     (treemacs-select-workspace-by-name candidate))
		   )
	:buffer "*helm treemacs*"))
;;; helm-treemacs ends

(use-package treemacs-projectile)

(use-package treemacs-evil)

(provide 'init-treemacs)
