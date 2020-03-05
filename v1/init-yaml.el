;;; yaml/init.el

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )


(provide 'init-yaml)
