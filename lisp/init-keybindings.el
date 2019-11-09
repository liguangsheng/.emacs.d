;; keybindings

(defun open-init-el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-idle-delay 0.4
	which-key-separator " → "
	which-key-prefix-prefix "+"
	which-key-side-window-max-heght 0.25)
  :config
  (which-key-add-key-based-replacements
    "SPC b" "buffer"
    "SPC c" "comment"
    "SPC e" "expand"
    "SPC f" "file"
    "SPC h" "helm"
    "SPC m" "mode"
    "SPC p" "projectile"
    "SPC q" "quit"
    "SPC t" "treemacs"
    "SPC w" "window"
    )
  (which-key-mode 1))

(bind-keys
 ("C-j" . ace-window)
 )

(bind-keys
 :map evil-normal-state-map
 ("J" . evil-scroll-page-down)
 ("K" . evil-scroll-page-up)
 ("U" . undo-tree-redo)
 ("gJ" . evil-join)
 )

(bind-keys
 :map evil-motion-state-map
 ("K" . evil-scroll-page-up))

(evil-leader/set-key
  ;; file
  "fw" 'save-buffer
  "fe" 'open-init-el
  "fs" 'save-buffer

  ;; buffer
  "bd"  'kill-this-buffer
  "bD"  'kill-all-buffers
  "bs"  'switch-to-scratch

  ;; window
  "wd"    'delete-window
  "wn"    'other-window
  "wo"    'delete-other-windows
  "w-"    'split-window-below
  "w|"    'split-window-right
  "ww"    'ace-window
  "w SPC" 'ace-window

  ;; comment
  "cl" 'comment-line
  "cc" 'comment-dwim

  ;; move
  "SPC"   'avy-goto-word-1
  "l"     'avy-goto-line        

  ;; quit
  "qq" 'save-buffers-kill-emacs
  )


(provide 'init-keybindings)
