;;; javascript/init.el

(use-package js2-mode
  :init (add-to-list 'auto-mode-alist '("\\.js" . js2-mode)))

(use-package json-mode
  :init (add-to-list 'auto-mode-alist '("\\.json" . json-mode)))

(use-package tern)

(use-package company-tern)

(use-package nodejs-repl)

(defun js2-mode-hook ()
  (add-to-list 'company-backends 'company-tern))

(add-hook 'js2-mode-hook 'js2-mode-hook)


(provide 'init-javascript)
