;;; init-vcs.el --- Version control system configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures version control tools including Magit, diff-hl,
;; git-messenger, and smerge for merge conflict resolution.

;;; Code:

;; Magit for Git operations
(use-package magit
  :init
  (evil-set-initial-state 'magit-mode 'emacs)
  :config
  (my-leader-def
   "MM" #'magit))

;; Highlight Git diffs in gutter
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'prog-mode-hook #'diff-hl-flydiff-mode))

;; Show Git commit messages for current line
(use-package git-messenger
  :init
  (setq git-messenger:show-detail t))

;; Smerge mode for merge conflict resolution
(use-package smerge-mode
  :custom
  (smerge-auto-leave t)
  :pretty-hydra
  ((:color red :quit-key "q" :title "Fix conflicts with smerge")
   ("Toggle"
    (("S" smerge-mode "Toggle smerge mode" :toggle t)))
   "Move"
   (("n" smerge-next "Next conflict")
    ("p" smerge-prev "Previous conflict")
    ("N" smerge-vc-next-conflict "Next conflict (VC)"))
   "Keep"
   (("l" smerge-keep-lower "Keep lower")
    ("u" smerge-keep-upper "Keep upper")
    ("a" smerge-keep-all "Keep all")
    ("b" smerge-keep-base "Keep base")
    ("c" smerge-keep-current "Keep current")))
  :general
  (my-leader-def
   "^" #'smerge-mode-hydra/body))

(provide 'init-vcs)

;;; init-vcs.el ends here
