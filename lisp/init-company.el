(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
		company-tooltip-limit 24            ; bigger popup window
		company-idle-delay 0.0              ; decrease delay before autocompletion popup shows
		company-echo-delay 0                ; remove annoying blinking
		company-minimum-prefix-length 1
		company-require-match nil
		company-dabbrev-ignore-case nil
		company-dabbrev-downcase nil)
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

(use-package company-tabnine)

(provide 'init-company)
