;; Mac OS: 
;; brew install llvm

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :config
  (c-add-style "microsoft"
	       '("stroustrup"
		 (c-offsets-alist
		  (innamespace . -)
		  (inline-open . 0)
		  (inher-cont . c-lineup-multi-inher)
		  (arglist-cont-nonempty . +)
		  (template-args-cont . +))))
  (setq c-default-style "microsoft")

  (use-package modern-cpp-font-lock
    :diminish
    :config (modern-c++-font-lock-global-mode t))
  )

(provide 'init-cc)
