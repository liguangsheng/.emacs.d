;; (let ((te-projects nil))
;;   (dolist (ws (treemacs-workspaces))
;;     (dolist (proj (treemacs-workspace->projects ws))
;;       (add-to-list 'te-projects (concat (treemacs-workspace->name ws) " -> " (treemacs-project->name proj)))
;;       (message (concat (treemacs-workspace->name ws) " -> " (treemacs-project->name proj)))
;;       ))
;;   te-projects)

;; (let ((te-projects nil))
;;   (dolist (ws (treemacs-workspaces))
;;     (dolist (proj (treemacs-workspace->projects ws))
;;       (add-to-list 'te-projects (concat (treemacs-workspace->name ws) " -> " (treemacs-project->name proj)))
;;       (message (concat (treemacs-workspace->name ws) " -> " (treemacs-project->name proj)))
;;       ))
;;   te-projects)



(provide 'init-dev)
