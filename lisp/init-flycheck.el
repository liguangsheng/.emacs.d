;;; init-flycheck.el --- Flycheck syntax checking -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Flycheck for syntax checking and linting.

;;; Code:

(use-package flycheck
  ;; Optional: Enable flycheck in specific modes
  ;; :init
  ;; (add-hooks (list 'go-mode-hook
  ;;                  'rust-mode-hook
  ;;                  'python-mode-hook
  ;;                  'c-mode-hook)
  ;;            #'flycheck-mode)
  )

(use-package flycheck-golangci-lint
  :after flycheck
  :hook (go-mode . flycheck-golangci-lint-setup))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
