;; 彩虹分隔符
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; 高亮缩进
(use-package highlight-indentation
  :disabled
  :config
  (add-hook 'prog-mode-hook #'highlight-indentation-current-column-mode))

;; 高亮数字
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; 高亮TODO
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; 高亮symbol
(use-package highlight-symbol
  :config
  (highlight-symbol-mode))

(provide 'init-highlight)
;;; init-highlight.el ends here

