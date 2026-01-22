;;; init-prog.el --- Programming modes configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures various programming language modes and their
;; associated settings for syntax highlighting, indentation, and formatting.

;;; Code:

;; Haskell
(use-package haskell-mode)

;; Ruby
(use-package ruby-mode :defer t)

;; Lua
(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode))

;; TypeScript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;; JSON
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

;; TOML
(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

;; PowerShell
(use-package powershell :defer t)

;; Bazel
(use-package bazel
  :mode (("WORKSPACE\\'" . bazel-mode)
         ("BUILD\\'"     . bazel-mode)))

;; GraphQL
(use-package graphql-mode
  :init
  ;; graphql-indent-line errors on blank lines, so ignore this indent function
  (add-to-list 'indent-line-ignored-functions #'graphql-indent-line)
  :config
  ;; Use format-all-buffer instead of my-format-buffer for formatting
  (define-key graphql-mode-map [remap my-format-buffer] #'format-all-buffer))

;; Protocol Buffers
(use-package protobuf-mode
  :config
  (my-leader-def "=" #'my-format-buffer))

;; Groovy
(use-package groovy-mode)

;; Dockerfile
(use-package dockerfile-mode)

;; YAML
(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode))
  :hook (yaml-mode . (lambda ()
                       (setq-local indent-tabs-mode nil)
                       (setq-local tab-width 2)
                       (setq-local indent-line-function 'yaml-indent-line))))

;; Makefile
(use-package make-mode
  :mode (("Makefile\\'" . makefile-mode)
         ("\\.mk\\'" . makefile-mode))
  :hook (makefile-mode . (lambda ()
                           (setq-local indent-tabs-mode t)
                           (setq-local tab-width 8)
                           (setq-local indent-line-function 'ignore))))

(provide 'init-prog)

;;; init-prog.el ends here

