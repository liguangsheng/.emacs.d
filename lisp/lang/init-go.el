;;; init-go.el --- Configuration for Golang

(use-package go-mode
  :init 
  ;; 环境变量
  (when (memq window-system '(mac ns x))
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
      (unless (getenv var)
	(exec-path-from-shell-copy-env var))))

  :config
  ;; 寻找goretuens作为格式化工具
  ;; go get -u -v github.com/sqs/goreturns
  (when (executable-find "goreturns")
    (setq gofmt-command "goreturns"))

  (add-hook 'go-mode-hook
	    (lambda ()
	      ;; 保存buffer之前格式化文件
	      (add-hook 'before-save-hook 'gofmt-before-save t)

	      ;; eyes and hands comfort
	      (subword-mode 1)
	      (setq tab-width 4)
	      (setq indent-tabs-mode 1)

	      (evil-leader/set-key
		"mdd" 'godef-describe)

	      (bind-key "s-]" 'godef-jump go-mode-map)
	      (bind-key "s-[" 'pop-tag-mark go-mode-map)

	      ;; Go support for lsp-mode using Sourcegraph's Go Language Server
	      ;; go get -u -v github.com/sourcegraph/go-langserver
	      (require 'lsp-go)
	      ))

  :bind (:map go-mode-map
	      ("s-[" . xref-find-definitions)
	      ("s-]" . xref-find-references)
	      )
  )

(use-package flycheck-golangci-lint
  :if (executable-find "golangci-lint")
  :after flycheck
  :defines flycheck-disabled-checkers
  :hook (go-mode . (lambda ()
		     "Enable golangci-lint."
		     (setq flycheck-disabled-checkers '(go-gofmt
							go-golint
							go-vet
							go-build
							go-test
							go-errcheck))
		     (flycheck-golangci-lint-setup))))

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

;; (use-package go-eldoc
;;   :hook (go-mode . go-eldoc-setup))

(use-package go-guru
  :bind (:map go-mode-map
	      ([remap xref-find-definitions] . go-guru-definition)
	      ([remap xref-find-references] . go-guru-referrers)))

(with-eval-after-load 'company
  (use-package company-go
    :defines company-backends
    :init (cl-pushnew 'company-go company-backends)))

(with-eval-after-load 'projectile
  (use-package go-projectile
    :commands (go-projectile-mode go-projectile-switch-project)
    :hook ((go-mode . go-projectile-mode)
	   (projectile-after-switch-project . go-projectile-switch-project))))

(use-package go-add-tags)
(use-package go-dlv)
(use-package go-fill-struct)
(use-package go-impl)
(use-package go-playground)
(use-package go-rename)
(use-package go-snippets)
(use-package golint)
(use-package govet)

(provide 'init-go)
;;; init-go.el ends here
