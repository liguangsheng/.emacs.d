;; 撤销
(use-package undo-tree)

;; 扩展选区
(use-package expand-region)
(use-package electric-operator
  :config
  (add-hook 'prog-mode #'electric-operator-mode))

;; Jump
(use-package avy)
(use-package ace-window)


(provide 'init-editing)
;;; init-editing ends here
