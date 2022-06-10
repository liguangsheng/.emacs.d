(use-package magit
  :init
  (evil-set-initial-state 'magit-mode 'emacs)

  :config
  (my-leader-def
	"MM" #'magit
	)
  )

;; 高亮git diff
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'prog-mode-hool #'diff-hl-flydiff-mode)
  )

;; popup git 修改记录
(use-package git-messenger
  :init
  (setq git-messenger:show-detail t))

(use-package smerge-mode
  :custom
  (smerge-auto-leave t)
  :pretty-hydra
  ((:color red :quit-key "q" :title "Fix conflicts with smerge")
   ("Toggle"
	(("S" smerge-mode "Toggle smerge mode" :toggle t))

	"Move"
	(("n" smerge-next "Next conflict")
	 ("p" smerge-prev "Previous conflict")
	 ("N" smerge-vc-next-conflict "Next conflict (VC)")
	 )

	"Keep"
	(("l" smerge-keep-lower "Keep lower")
	 ("u" smerge-keep-upper "Keep upper")
	 ("a" smerge-keep-all "Keep all")
	 ("b" smerge-keep-base "Keep base")
	 ("c" smerge-keep-current "Keep current"))))

  :general
  (my-leader-def
	"^" #'smerge-mode-hydra/body)
  )

(provide 'init-vcs)
