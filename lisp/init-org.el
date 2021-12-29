(use-package org-bullets
  :defer t
  :init
  (setq
   org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "ABORT(a)"))
   org-todo-keyword-faces '(("TODO" . "red")
			    ("DOING" . "yellow")
			    ("DONE" . "green")
			    ("ABORT" . "blue")))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode 1)
	      ;; 在org-table中使用中英文等宽的字体使表格框线对齐
	      (set-face-attribute 'org-table nil 
				  ;; :family "Noto Sans Mono CJK SC"
				  :family "Iosevka"
				  :weight 'normal
				  :width 'normal))))

(provide 'init-org)
