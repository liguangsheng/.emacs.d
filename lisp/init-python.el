(use-package python-mode
  :init
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  :hook (python-mode . (lambda () (eldoc-mode 0))))

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-pyright)
;; 			 (lsp-deferred))))  ; or lsp-deferred


(provide 'init-python)
