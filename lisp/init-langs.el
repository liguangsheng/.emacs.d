;;; init-langs.el -- language configuration
;;; Commentary:
;;; Code:

;;; Haskell:
(use-package haskell-mode)

;;; Ruby:
(use-package ruby-mode :defer t)

(use-package lua-mode
  :defer t
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode))

(use-package typescript-mode
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)
	 ("\\.tsx\\'" . typescript-mode)))

(use-package json-mode
  :defer t
  :mode (("\\.json\\'" . json-mode)))

(use-package toml-mode
  :defer t
  :mode (("\\.toml\\'" . toml-mode)))

(use-package yaml-mode
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(use-package powershell :defer t)

(use-package bazel
  :defer t
  :mode (("WORKSPACE\\'" . bazel-mode)
	 ("BUILD\\'"     . bazel-mode)))

(use-package graphql-mode)

(use-package protobuf-mode)

(use-package groovy-mode)

(use-package dockerfile-mode)

(provide 'init-langs)
;;; init-langs.el ends here
