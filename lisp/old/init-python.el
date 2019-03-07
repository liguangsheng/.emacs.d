;;; python/init.el

(defvar python-path "/usr/local/bin/python")
(defvar python-lsp t)

(use-package python-mode
  :diminish "Python"
  :config
  (if python-lsp-p
      (add-hook 'python-mode-hook 'lsp)
    (progn
      (use-package anaconda-mode
	:hook ((python-mode . anaconda-mode)
	       (python-mode . anaconda-eldoc-mode))
	:init (setq anaconda-mode-installation-directory (concat emacs-cache-dir "anaconda-mode/"))
	:bind (:map evil-normal-state-map
		    ("M-<up>" . anaconda-mode-find-definitions)
		    ("M-<down>" . anaconda-mode-go-back)
		    ("M-<left>" . anaconda-mode-find-assignments)
		    ("M-<right>" . anaconda-mode-go-back)
		    :map python-mode-map
		    ("M-/" . anaconda-mode-complete)))

      (use-package company-anaconda
	:config (add-to-list 'company-backends 'company-anaconda)))))


(defun python-pop ()
  "Run python and switch to the python buffer.
similar to shell-pop"
  (interactive)
  (if (get-buffer "*Python*")
      (if (string= (buffer-name) "*Python*")
	  (if (not (one-window-p))
	      (progn (bury-buffer)
		     (delete-window))
	    )
	(progn (switch-to-buffer-other-window "*Python*")
	       (end-of-buffer)
	       (evil-insert-state)))
    (progn
      (run-python)
      (switch-to-buffer-other-window "*Python*")
      (end-of-buffer)
      (evil-insert-state))))

(provide 'init-python)
