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

;; (use-package smex)
;; (use-package ido ;;   :init
;;   (ido-mode 1)
;;   :config
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-default-file-method 'selected-window)
;;   (setq ido-default-buffer-method 'selected-window)

;;   (global-set-key
;;    "\M-x"
;;    (lambda ()
;;      (interactive)
;;      (call-interactively
;;       (intern
;;        (ido-completing-read
;;         "M-x "
;;         (all-completions "" obarray 'commandp))))))
;;   )

;; (use-package ido-vertical-mode
;;   :init
;;   (ido-vertical-mode 1)
;;   :config
;;   (setq ido-vertical-show-count t))

;; (use-package ido-sort-mtime
;;   :hook (ido-mode . ido-sort-mtime-mode))

;; (use-package crm-custom
;;   :hook (ido-mode . crm-custom-mode))

;; (use-package flx-ido
;;   :hook (ido-mode . flx-ido-mode))

;; (use-package rg)
;; (use-package ag)

;; ;;; Helm
;; (use-package helm
;;   :bind
;;   ([remap execute-extended-command] . helm-M-x)
;;   ([remap find-file]                . helm-find-files)
;;   ([remap find-dir]                 . dired)
;;   ;; ([remap recentf-open-files]       . helm-recentf)
;;   ;; ([remap imenu]                    . helm-imenu)
;;   )

;; (use-package helm-company
;;   :after (helm company))

;; (use-package helm-swoop)

;; (use-package helm-rg)

;; (use-package helm-ag)

;; (use-package helm-projectile
;;   :bind
;;   ([remap projectile-switch-project] . helm-projectile-switch-project)
;;   :init (helm-projectile-on))

;;; Vertico

(use-package vertico
  :init
  (setq vertico-count 20)
  (vertico-mode)
  :config
  (define-key vertico-map "?" #'minibuffer-completion-help)
  (define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
  (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)

  (setq completion-styles '(substring orderless))
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico-posframe
  :if perferences/enable-posframe
  :init
  (setq vertico-posframe-font perferences/font
	vertico-posframe-height 20)
  (vertico-posframe-mode 1))

(use-package consult
  :bind
  ([remap switch-to-buffer]         . consult-buffer)
  ([remap recentf-open-files]       . consult-recent-file)
  ([remap bookmark-jump]            . consult-bookmark)
  ([remap imenu]                    . consult-imenu)
  :general
  (my-leader-def
    "s p" 'consult-ripgrep)

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq  consult-narrow-key "<"
         consult-line-numbers-widen t
         consult-async-min-input 2
         consult-async-refresh-delay  0.15
         consult-async-input-throttle 0.2
         consult-async-input-debounce 0.1)

  :config
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  )

;; (use-package consult-projectile
;;   :bind
;;   ([remap projectile-find-file] . consult-projectile))

;; (use-package marginalia)

(provide 'init-completion)
