;;; Company

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
	company-dabbrev-downcase nil
	company-show-numbers t
	)

  (setq company-backends '(company-dabbrev
			   company-keywords
			   company-files
			   company-capf))
  )

(use-package company-quickhelp
  :after (company)
  :init
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

(use-package company-tabnine
  :hook (prog-mode . (lambda () (push 'company-tabnine company-backends))))
;; 在编辑器里可以直接打出TabNine相关的命令
;; TabNine::sem: 开启TabNine基于semantic的补全
;; TabNine::config_dir: 显示配置文件夹

(provide 'init-company)