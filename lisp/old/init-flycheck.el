(use-package flycheck
  :init
  ;; (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-flake8-maximum-line-length 80
        flycheck-flake8-maximum-complexity 10
        ))

(use-package flycheck-color-mode-line
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(provide 'init-flycheck)
