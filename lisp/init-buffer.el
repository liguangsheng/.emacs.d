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

(my-leader-def
  "b"   '(:ignore t :wk "buffer")
  "bD" '(kill-buffer :wk "Kill buffer")
  "bR" '(rename-buffer-file :wk "Rename buffer file")
  "bY" '(yank-whole-buffer :wk "Yank whole buffer")
  "bb" '(switch-to-buffer :wk "Switch buffer")
  "bd" '(:ignore t :wk "delete buffer in group")
  "bdd" #'centaur-tabs-kill-match-buffers-in-current-group
  "bdo" #'centaur-tabs-kill-other-buffers-in-current-group
  "br" 'revert-buffer
  "bx" '(kill-this-buffer :wk "Kill this buffer")
  )

(provide 'init-buffer)
