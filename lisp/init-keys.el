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
  (which-key-mode 1)) 

(use-package which-key-posframe
  :if perferences/enable-posframe
  :init
  (setq which-key-posframe-font perferences/font)
  (which-key-posframe-mode 1))

(use-package general)

(general-create-definer my-leader-def
  :states '(normal insert visual emacs)
  :prefix my-leader-key
  :non-normal-prefix my-leader-alt-key)

(my-leader-def
  "SPC" '(projectile-find-file     :wk "Find file in project")
  "RET" '(bookmark-jump            :wk "Jump to bookmark")
  ":"   '(execute-extended-command :wk "M-x")
  ";"   '(eval-last-sexp           :wk "Eval last sexp")
  "`"   '(evil-switch-to-windows-last-buffer
	  :wk "Switch to last buffer")
  
  ;; buffer
  "b" '(:ignore t :wk "buffer")
  "b b" '(switch-to-buffer :wk "Switch buffer")
  "b d" '(kill-this-buffer :wk "Kill this buffer")
  "b D" '(kill-buffer      :wk "Kill buffer")

  ;; code
  "c" '(:ignore t :wk "code")
  "c d" '(xref-find-definitions   :wk "Jump to definitions")
  "c D" '(xref-find-references    :wk "Jump to references")
  "c f" '(indent-whole-buffer     :wk "Format buffer")
  "c i" '(imenu                   :wk "Imenu")
  "c I" '(lsp-goto-implementation :wk "Jump to implementation")

  ;; file
  "f" '(:ignore t :wk "file")
  "f e" '(open-init-el       :wk "open ~/.emacs.d/init.el")
  "f f" '(find-file          :wk "Find file")
  "f d" '(find-dir           :wk "Find file")
  "f r" '(recentf-open-files :wk "Recent files")
  "f F" '(my/find-file-from-here :wk "Find file from here")

  ;; project
  "p" '(:ignore t :wk "+project")
  "p p" '(projectile-switch-project              :wk "Switch project")
  "p f" '(projectile-find-file                   :wk "Find file in project")
  "p i" '(projectile-invalidate-cache            :wk "Invalidate project cache")
  "p 4" '(projectile-find-file-dwim-other-window :wk "Find project file in other window")
  "p 5" '(projectile-find-file-dwim-other-frame  :wk "Find project file in other frame")
  "p d" '(projectile-find-dir                    :wk "Find dir in project")

  ;; quit
  "q" '(:ignore t :wk "quit")
  "q Q" '(kill-emacs    :wk "Quit emacs")
  "q r" '(restart-emacs :wk "Restart emacs")

  ;; search
  "s" '(:ignore t :wk "search")
  "s b" '(consult-line       :wk "Search buffer")
  "s s" '(consult-line       :wk "Search buffer")
  "s p" '(consult-ripgrep    :wk "Search project")
  "s g" '(vc-git-grep        :wk "Search by git grep")
  "s i" '(imenu              :wk "Jump to symbol")

  ;; window
  "w" '(:ignore t :wk "window")
  "w w" 'ace-window
  "w |" 'split-window-horizontally
  "w -" 'split-window-vertically
  "w d" 'delete-window
  "w D" 'ace-delete-window
  "w o" 'delete-other-windows
  )

(provide 'init-keys)
