(use-package restart-emacs
  :config
  (evil-leader/set-key "qr" 'restart-emacs))

(defun kill-all-buffers ()
  (interactive)
  (kill-buffers nil))



(provide 'init-misc)
