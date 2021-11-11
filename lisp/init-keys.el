(defvar my-leader-key "SPC")

(defvar my-leader-alt-key "C-,")

(defvar my-localleader-key "SPC m")

(defvar my-localleader-alt-key "C-, m")


(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-idle-delay 0.4
	which-key-separator " → "
	which-key-prefix-prefix "+"
	which-key-side-window-max-heght 0.25)
  (which-key-setup-minibuffer)
  :config
  (which-key-mode 1)) 

(use-package general)

(general-create-definer my-leader-def
  :states '(normal insert visual emacs)
  :prefix my-leader-key
  :non-normal-prefix my-leader-alt-key)

(my-leader-def
  "SPC" '(projectile-find-file :which-key "Find file in project")
  
  ;; buffer
  "b" '(:ignore t :which-key "+buffer")
  "b b" '(helm-buffers-list :wk "Switch buffer")
  "b d" '(kill-buffer :wk "Kill this buffer")

  ;; code
  "c" '(:ignore t :which-key "+code")
  "c d" '(xref-find-definition :which-key "Jump to definitions")
  "c D" '(xref-find-references :which-key "Jump o references")
  "c f" '(indent-whole-buffer  :which-key "Format buffer")
  "c i" '(imenu                :which-key "Imenu")

  ;; file
  "f" '(:ignore t :which-key "+file")
  "f e" '(open-init-el       :wk "open ~/.emacs.d/init.el")
  "f f" '(find-file          :wk "Find file")
  "f d" '(find-dir           :wk "Find file")
  "f r" '(recentf-open-files :wk "Recent files")
  "f F" '(my/find-file-from-here :wk "Find file from here")

  ;; project
  "p" '(:ignore t :which-key "+project")
  "p p" '(projectile-switch-project              :which-key "Switch project")
  "p f" '(projectile-find-file                   :which-key "Find file in project")
  "p i" '(projectile-invalidate-cache            :which-key "Invalidate project cache")
  "p o" '(projectile-find-file-dwim-other-window :which-key "Find other file")
  "p d" '(projectile-find-dir                    :which-key "Find dir in project")

  ;; quit
  "q" '(:ignore t :which-key "+quit")
  "q Q" '(kill-emacs    :which-key "Quit emacs")
  "q r" '(restart-emacs :which-key "Restart emacs")

  ;; search
  "s" '(:ignore t :which-key "+search")
  "s b" '(helm-swoop         :which-key "Search buffer")
  "s s" '(helm-swoop         :which-key "Search buffer")
  "s p" '(helm-projectile-ag :which-key "Search project")

  ;; window
  "w" '(:ignore t :which-key "+window")
  "w w" 'ace-window
  "w |" 'split-window-horizontally
  "w -" 'split-window-vertically
  "w d" 'delete-window
  "w D" 'ace-delete-window
  "w o" 'delete-other-windows
  )

(provide 'init-keys)
