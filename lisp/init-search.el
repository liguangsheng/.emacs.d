(use-package wgrep)
(use-package rg)

(use-package color-rg
  :straight (el-patch :type git :host github :repo "manateelazycat/color-rg")
  :init
  (require 'color-rg))

(provide 'init-search)
