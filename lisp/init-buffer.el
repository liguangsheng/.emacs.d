(defun yank-whole-buffer ()
  "Yank the entire buffer without changing the point position."
  (interactive)
  (let ((original-point (point)))
    (clipboard-kill-ring-save (point-min) (point-max))
    (goto-char original-point)
    (message "Whole buffer copied to kill ring")))

(defun rename-buffer-file (new-name)
  "Write the content of the current buffer to a new file and delete the old file."
  (interactive "FNew name: ")
  (let* ((old-name (buffer-file-name))
         (buffer-contents (buffer-string)))
    (if old-name
        (progn
          (write-region buffer-contents nil new-name)
          (delete-file old-name)
          (set-visited-file-name new-name t t)
          (message "File renamed to %s" new-name))
      (error "Buffer is not visiting a file"))))

(defun clean-idle-buffers ()
  "Close buffers that have been open for more than 15 minutes and have not been edited.
Skip buffers with names like *Message*."
  (interactive)
  (let* ((tm (current-time))
		 (ts (format-time-string "%Y-%m-%d %T" tm))
		 (bc 0)
		 (kbc 0)
		 bts delay cbld bn)
    (dolist (buf (buffer-list))
	  (cl-incf bc)
	  (when (buffer-live-p buf)
		(setq bts (with-current-buffer buf buffer-display-time)
			  bn (buffer-name buf)
			  delay (if bts (round (float-time (time-subtract tm bts))) 0)
			  cbld (* 15 60))
		(message "[%s] `%s' [%s %d]" ts bn delay cbld)
		(unless (or (string-match-p "\\*\\(.*\\)\\*" bn)
					(get-buffer-process buf)
					(and (buffer-file-name buf) (buffer-modified-p buf))
					(get-buffer-window buf 'visible)
					(< delay cbld))
		  (message "[%s] [%d/%d] killing `%s'" ts kbc bc bn)
		  (cl-incf kbc)
		  (kill-buffer buf))))
	(message "[%s] %d/%d buffers killed." ts kbc bc)))

(my-leader-def
  "b"   '(:ignore t :wk "buffer")
  "bD" '(kill-buffer :wk "Kill buffer")
  "bR" '(rename-buffer-file :wk "Rename buffer file")
  "bY" '(yank-whole-buffer :wk "Yank whole buffer")
  "bb" '(switch-to-buffer :wk "Switch buffer")
  "bd" '(:ignore t :wk "delete buffer in group")
  "bdd" #'centaur-tabs-kill-match-buffers-in-current-group
  "bdo" #'centaur-tabs-kill-other-buffers-in-current-group
  "bdi" '(clean-idle-buffers :wk "Clean idle buffers")
  "br" '(revert-buffer :wk "Revert current buffer")
  "bx" '(kill-current-buffer :wk "Kill current buffer")
  )

(provide 'init-buffer)
