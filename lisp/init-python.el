(use-package python-mode
  :config
  ;; 使用format-all-buffer替代my-format-buffer进行格式化
  (define-key graphql-mode-map [remap my-format-buffer] #'format-all-buffer))

(provide 'init-python)
