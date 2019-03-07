;; keybindings

(defun open-init-el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(use-package restart-emacs)

(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 0.4
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-side-window-max-heght 0.25)
  :config
  (which-key-mode 1))

(bind-keys
 ("M-x" . helm-M-x)
 ("C-j" . ace-window)
 )

(bind-keys
 :map evil-normal-state-map
 ("J" . evil-scroll-page-down)
 ("K" . evil-scroll-page-up)
 ("U" . undo-tree-redo)
 ("<f2>" . neotree-toggle)
 )

(bind-keys
 :map evil-motion-state-map
 ("K" . evil-scroll-page-up))

(evil-leader/set-key
  ;; file
  "fw" 'save-buffer
  "ff" 'helm-find-files
  "fr" 'helm-recentf
  "fe" 'open-init-el
  "fs" 'save-buffer
  "ft" 'neotree-show

  ;; buffer
  "bb"  'helm-mini
  "bd"  'kill-this-buffer
  "bD"  'kill-all-buffers
  "bh"  'switch-to-dashboard
  "bs"  'switch-to-scratch

  ;; window
  "wd"    'delete-window
  "wn"    'other-window
  "wo"    'delete-other-windows
  "w-"    'split-window-below
  "w|"    'split-window-right
  "ww"    'ace-window
  "w spc" 'ace-window

  ;; comment
  "cl"  'comment-line

  ;; move & jump
  "SPC"   'avy-goto-word-1
  "l"     'avy-goto-line        

  ;; toggle
  "tn"    'toggle-relative-mode

  ;; helm
  "hi"   'helm-imenu
  "hp"   'helm-projectile
  "hx"   'helm-m-x

  ;; quit
  "qq" 'save-buffers-kill-emacs
  "qr" 'restart-emacs

  ;; project
  "pf" 'helm-projectile-find-file-dwim
  "pg" 'helm-projectile-grep
  "pb" 'helm-projectile-switch-to-buffer
  "pr" 'helm-projectile-recentf
  
  ;; other
  "v"  'er/expand-region
  "tn" 'reload-theme
  )

(which-key-add-key-based-replacements
  "SPC b" "buffer"
  "SPC c" "comment"
  "SPC f" "file"
  "SPC h" "help"
  "SPC m" "mode"
  "SPC q" "quit"
  "SPC t" "toggle"
  "SPC w" "window"
  )

(provide 'init-keybindings)
