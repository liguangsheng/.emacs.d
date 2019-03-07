;;; quickrun/init.el

(use-package quickrun
  :init
  (defun my-quickrun ()
    (interactive)
    (if (use-region-p)
	(quickrun-region (region-beginning) (region-end))
      (quickrun)))
  :config
  (bind-key "<f5>" 'my-quickrun))

(provide 'init-quickrun)
