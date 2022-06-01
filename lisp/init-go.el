(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . go-mode-hook-func))
  :bind (:map go-mode-map
	      ("C-c d d" . godef-describe)
	      ("C-c d p" . godoc-at-point)
	      ("C-c r u" . go-remove-unused-imports)
	      ("C-M-l" . gofmt))
  :init
  ;; Copy system environment variables
  (when (memq window-system '(mac ns))
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
      (unless (getenv var)
	(exec-path-from-shell-copy-env var))))

  (defun go-mode-hook-func ()
    (subword-mode 1)
    (setq tab-width 4
	  indent-tabs-mode 1)
    (with-eval-after-load 'format-all
      (setq  format-all-formatters '(("Go" goimports))))
    )
  )

(use-package go-tag
  :bind (:map go-mode-map
	      ("C-c t" . go-tag-add)
	      ("C-c T" . go-tag-remove))
  :config (setq go-tag-args (list "-transform" "camelcase")))

(use-package gotest
  :bind (:map go-mode-map
	      ("C-c a" . go-test-current-project)
	      ("C-c m" . go-test-current-file)
	      ("C-c ." . go-test-current-test)
	      ("C-c x" . go-run)))

(use-package go-gen-test
  :bind (:map go-mode-map
	      ("C-c C-t" . go-gen-test-dwim)))

(use-package go-projectile
  :after projectile
  :commands (go-projectile-mode go-projectile-switch-project)
  :hook ((go-mode . go-projectile-mode)
	 (projectile-after-switch-project . go-projectile-switch-project)))

(use-package go-add-tags)
(use-package go-dlv)
(use-package go-impl)
(use-package go-playground)
(use-package go-snippets)
(use-package golint)

(provide 'init-go)
