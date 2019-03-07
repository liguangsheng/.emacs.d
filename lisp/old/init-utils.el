(defun ensure-dir (DIR)
  (if (not (file-exists-p DIR)) (make-directory DIR)))

(defun windows-p ()
  (eq system-type 'windows-nt))

(defun linux-p ()
  (eq system-type 'gnu/linux))

(defun mac-os-p ()
  (eq system-type 'darwin))

(defun kill-buffers (KILL-STAR-BUFFER)
  (dolist (buffer (buffer-list))
    (let ((bname (string-trim (buffer-name buffer))))
      (if (and (not KILL-STAR-BUFFER)
               (string-prefix-p "*" bname)
               (string-suffix-p "*" bname))
          nil
        (kill-buffer buffer)
        ))))

(defun kill-all-buffers ()
  (interactive)
  (kill-buffers nil))

(defun open-emacs-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun switch-to-dashboard ()
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun list-random-choice (list)
  (nth (random (list-length list)) list))

(provide 'init-utils)
