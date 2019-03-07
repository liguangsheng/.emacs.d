(use-package all-the-icons)

(setq frame-title-format (list "LGS-Emacs: "
			       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(use-package diminish
  :config
  (progn
    (diminish 'git-gutter-mode  "\u24d6")
    (diminish 'undo-tree-mode   "\u24e4")
    (diminish 'company-mode     "\u24d2")
    (diminish 'yas-minor-mode   "\u24e8")
    (diminish 'auto-revert-mode "\u24d0")
    (diminish 'which-key-mode   "\u24e6")
    (diminish 'smartparens-mode "\u24e2")
    (diminish 'message-mode     "\u24c2")
    (diminish 'eldoc-mode       "\u24d4")
    ))

(use-package minions
  :config (minions-mode 1))

;; spaceline
(use-package all-the-icons)

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
