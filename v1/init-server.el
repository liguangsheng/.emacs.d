;; start server
(defvar emacs-server-p t
  "Do you want start a emacs server.")

(defun server-socket-path ()
  (concat server-socket-dir server-name))

(defun emacs-server-exist-p ()
  (file-exists-p (server-socket-path)))

(defun restart-emacs-server ()
  (interactive)
  (server-force-delete)
  (server-start))

(when emacs-server-p
  (restart-emacs-server))

(provide 'init-server)
