(use-package flycheck
  :init
  (add-hooks #'flycheck-mode
	     (list 'go-mode-hook
		   'rust-mode-hook
		   'python-mode-hook
		   'c-mode-hook)))

(use-package flycheck-golangci-lint
  :after flycheck
  :hook (go-mode . flycheck-golangci-lint-setup))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
