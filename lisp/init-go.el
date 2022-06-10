(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . my-go-mode-hook))
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

  (defun my-go-mode-hook()
	(message "execute go-mode-hook")
    (subword-mode 1)
    (setq-local tab-width 4)
    (with-eval-after-load 'format-all
      (setq  format-all-formatters '(("Go" goimports))))

	(setq lsp-go-env '((GOFLAGS . "-tags=wireinject")))

	(lsp-deferred)
	(my-leader-def :keymaps 'local
	  "=" (lambda () (interactive) (format-all-buffer))
      )
	)
  )

(use-package go-tag
  :bind (:map go-mode-map
			  ("C-c t" . go-tag-add)
			  ("C-c T" . go-tag-remove))
  :custom
  (go-tag-args '("-transform" "camelcase")))

(use-package gotest
  :commands (go-test-current-project
			 go-test-current-file
			 go-test-current-test
			 go-test-current-benchmark
			 go-run)
  :bind (:map go-mode-map
			  ("C-x p" . go-test-current-project)
			  ("C-x f" . go-test-current-file)
			  ("C-x t" . go-test-current-test)
			  ("C-x b" . go-test-current-benchmark)
			  ("C-x x" . go-run))
  :init
  (add-hook 'go-mode-hook
			(lambda ()
			  (my-local-leader-def :keymaps 'local
				"t t" #'go-test-current-test
				"t b" #'go-test-current-benchmark
				"t f" #'go-test-current-file
				"t p" #'go-test-current-project
				"t r" #'go-run)
			  (setq-local go-test-args "-v -count=1")
			  ))
  )

(use-package go-dlv)
(use-package go-impl)
(use-package go-playground
  :diminish
  :commands (go-playground-mode))
(use-package go-snippets)
(use-package golint)

(defun paste-stacktrace ()
  "Paste text from the clipboard, replacing \\n with \n and \\t with \t.
Then insert the processed text at the current point."
  (interactive)
  (let* ((clipboard-text (gui-get-primary-selection))
         (processed-text
          (replace-regexp-in-string
           "\\\\n" "\n"
           (replace-regexp-in-string
            "\\\\t" "\t"
            clipboard-text))))
    (insert processed-text)))


(provide 'init-go)
