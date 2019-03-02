(use-package magit)

;; 高亮git diff
(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; popup git 修改记录
(use-package git-messenger
  :init
  (setq git-messenger:show-detail t))

(provide 'init-vcs)
