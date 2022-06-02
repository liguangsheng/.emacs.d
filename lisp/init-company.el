;;; Company

(use-package company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 24            ; bigger popup window
	company-idle-delay 0.0              ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 1
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil
	company-show-numbers t)
  (setq company-backends '(company-capf
			   company-keywords
			   company-files
			   company-dabbrev)))

(use-package company-quickhelp
  :after (company)
  :init
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

(use-package company-tabnine
  :after (company)
  :init
  (defun add-tabnine-to-company ()
    (push 'company-tabnine company-backends))
  (add-hook 'graphql-mode-hook #'add-tabnine-to-company)
  (add-hook 'protobuf-mode-hook #'add-tabnine-to-company)
  )
;; 在编辑器里可以直接打出TabNine相关的命令
;; TabNine::sem: 开启TabNine基于semantic的补全
;; TabNine::config_dir: 显示配置文件夹

(provide 'init-company)
