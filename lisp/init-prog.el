(use-package haskell-mode)

(use-package ruby-mode :defer t)

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
		 ("\\.tsx\\'" . typescript-mode)))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(use-package powershell :defer t)

(use-package bazel
  :mode (("WORKSPACE\\'" . bazel-mode)
		 ("BUILD\\'"     . bazel-mode)))

(use-package graphql-mode
  :init
  ;; graphql-indent-line在空白行处缩进会报错，所以要忽略这个indent函数
  (add-to-list 'indent-line-ignored-functions #'graphql-indent-line)

  :config
  ;; 使用format-all-buffer替代my-format-buffer进行格式化
  (define-key graphql-mode-map [remap my-format-buffer] #'format-all-buffer))

(use-package protobuf-mode
  :config
  (my-leader-def "=" #'my-format-buffer))

(use-package groovy-mode)

(use-package dockerfile-mode)

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode))
  :hook (yaml-mode . (lambda ()
					   (setq-local indent-tabs-mode nil)
					   (setq-local tab-width 2)
					   (setq-local indent-line-function 'yaml-indent-line))))

(use-package make-mode
  :mode (("Makefile\\'" . makefile-mode)
		 ("\\.mk\\'" . makefile-mode))
  :hook (makefile-mode . (lambda ()
						   (setq-local indent-tabs-mode t)
						   (setq-local tab-width 8)
						   (setq-local indent-line-function 'ignore))))


(provide 'init-prog)

