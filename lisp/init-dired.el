(use-package dired
  :straight nil
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies  'always
	dired-kill-when-opening-new-dired-buffer t
	)
  )

(use-package dired-single
  :config
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory)
  )

(provide 'init-dired)
