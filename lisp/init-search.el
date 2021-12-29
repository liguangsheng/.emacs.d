(use-package wgrep)

(use-package rg)

(use-package color-rg
  :straight '(color-rg :type git
		       :host github
		       :repo "manateelazycat/color-rg")
  :init
  (require 'color-rg)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'color-rg-mode 'insert))
  )

(provide 'init-search)
