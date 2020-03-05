;; 智能括号
(use-package smartparens
  :config
  (sp-with-modes
      '(c++-mode objc-mode c-mode go-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (smartparens-global-mode))

(provide 'init-pairs)
;;; init-pairs ends here
