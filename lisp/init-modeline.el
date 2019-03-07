(use-package doom-modeline
  :init (setq doom-modeline-height 25
	      doom-modeline-bar 3
	      doom-modeline-buffer-file-name-style 'relative-to-project
	      doom-modeline-icon t
	      doom-modeline-major-mode-icon t
	      )
  :config (doom-modeline-mode 1))

(provide 'init-modeline)
;;; init-modeline.el ends here
