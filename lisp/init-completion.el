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

;;; Helm
(use-package helm
  :bind
  ([remap execute-extended-command] . helm-M-x)
  ([remap find-file]                . helm-find-files)
  ([remap find-dir]                 . dired)
  ([remap recentf-open-files]       . helm-recentf)
  ([remap imenu]                    . helm-imenu)
  )

(use-package helm-company
  :after (helm company))

(use-package helm-swoop)

(use-package rg)
(use-package helm-rg)

(use-package ag)
(use-package helm-ag)

(use-package helm-projectile
  :bind
  ([remap projectile-switch-project] . helm-projectile-switch-project)
  :init (helm-projectile-on))

;;; Vertico
(use-package vertico
  :config (vertico-mode +1))

(use-package consult)

(use-package marginalia)

(provide 'init-completion)
