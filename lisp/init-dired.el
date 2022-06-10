(when (executable-find "gls")
  (setq insert-directory-program "gls"))

(use-package dired
  :straight nil
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies  'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)

  :general
  (my-leader-def "fd" 'dired)
  (:states 'normal :keymaps 'dired-mode-map
		   "SPC" nil))

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; Quick sort dired buffers via hydra
;; (use-package dired-quick-sort
;;   :general
;;   (:states 'normal :keymaps 'dired-mode-map
;; 		   "S" 'hydra-dired-quick-sort/body))

;; Show git info in dired
;; (use-package dired-git-info
;;   :general
;;   (:states 'normal :keymaps 'dired-mode-map
;; 		   ")" 'dired-git-info-mode))

;; Colorful dired
;; (use-package diredfl
;;   :hook (dired-mode . diredfl-mode))

;; (use-package dired-subtree
;;   :general
;;   (:states 'normal :keymaps 'dired-mode-map
;; 		   "TAB" 'dired-subtree-cycle))

;; (use-package fd-dired)

;; (use-package dired-k
;;   :general
;;   (:states 'normal :keymaps 'dired-mode-map
;; 		   "K" 'dired-k))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  (add-hook 'dirvish-find-entry-hook
			(lambda (&rest _) (setq-local truncate-lines t)))

  (when (executable-find "exa")
	(dirvish-define-preview exa (file)
	  "Use `exa' to generate directory preview."
	  :require ("exa") ; tell Dirvish to check if we have the executable
	  (when (file-directory-p file) ; we only interest in directories here
		`(shell . ("exa" "-al" "--color=always" "--icons"
				   "--group-directories-first" ,file))))

	(add-to-list 'dirvish-preview-dispatchers 'exa)
	)

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd))

  :general
  (:states 'normal :keymaps 'dirvish-mode-map ; Dirvish inherits `dired-mode-map'
		   "?"    #'dirvish-dispatch
		   "q"    #'dirvish-quit
		   "a"    #'dirvish-quick-access
		   "f"    #'dirvish-file-info-menu
		   "y"    #'dirvish-yank-menu
		   "N"    #'dirvish-narrow
		   "^"    #'dirvish-history-last
		   "h"    #'dirvish-history-jump ; remapped `describe-mode'
		   "s"    #'dirvish-quicksort    ; remapped `dired-sort-toggle-or-edit'
		   "v"    #'dirvish-vc-menu      ; remapped `dired-view-file'
		   "TAB"  #'dirvish-subtree-toggle
		   "M-f"  #'dirvish-history-go-forward
		   "M-b"  #'dirvish-history-go-backward
		   "M-l"  #'dirvish-ls-switches-menu
		   "M-m"  #'dirvish-mark-menu
		   "M-t"  #'dirvish-layout-toggle
		   "M-s"  #'dirvish-setup-menu
		   "M-e"  #'dirvish-emerge-menu
		   "M-j"  #'dirvish-fd-jump
		   "<mouse-1>" #'dirvish-subtree-toggle))

(use-package dired-x
  :straight nil
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package vscode-icon
  :config
  (push '("jpg" . "image") vscode-icon-file-alist))

(use-package dirvish-side
  :straight nil
  :custom
  (dirvish-side-width 42)

  :general
  (my-leader-def "fs" #'dirvish-side)
  )

(provide 'init-dired)
