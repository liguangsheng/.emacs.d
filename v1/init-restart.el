;; Restart Emacs
(use-package restart-emacs
  :commands (restart-emacs)
  :init
  (defun ask-restart-emacs ()
    (interactive)
    (when (y-or-n-p "Do you really want to restart Emacs? ")
      (restart-emacs))))


(provide 'init-restart)
;; init-restart.el ends here
