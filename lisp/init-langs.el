;;; init-langs.el -- language configuration
;;; Commentary:
;;; Code:

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

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(use-package powershell :defer t)

(use-package bazel
  :mode (("WORKSPACE\\'" . bazel-mode)
	 ("BUILD\\'"     . bazel-mode)))

(use-package graphql-mode)

(use-package protobuf-mode)

(use-package groovy-mode)

(use-package dockerfile-mode)

(provide 'init-langs)
;;; init-langs.el ends here
