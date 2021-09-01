;;; init-dired.el -- dired configuration

;;; Commentary:

;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always))

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
;;; init-dired.el ends here
