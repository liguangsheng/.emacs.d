(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :commands copilot-mode
  :hook (prog-mode . copilot-mode)

  :init
  (defun my/copilot-tab ()
	(interactive)
	(or (copilot-accept-completion)
		(corfu-complete)))

  :general
  (:states 'insert :kaymaps 'prog-mode
		   ;; "TAB" #'my/copilot-tab
		   "M-c" #'copilot-accept-completion
		   "M-l" #'copilot-clear-overlay
		   ))

(provide 'init-copilot)
