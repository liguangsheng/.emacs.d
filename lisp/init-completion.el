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

;;; Vertico

(use-package vertico
  :straight 
  '(vertico
    :files (:defaults "extensions/*")
    :includes (vertico-buffer
               vertico-directory
               vertico-flat
               vertico-indexed
               vertico-mouse
               vertico-quick
               vertico-repeat
               vertico-reverse))
  :init
  (setq vertico-count 20)
  (vertico-mode)
  :config
  (define-key vertico-map "?" #'minibuffer-completion-help)
  (define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
  (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)

  (setq completion-styles '(substring orderless))
  )

(use-package vertico-directory
  :straight nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-indexed
  :straight nil
  :init (vertico-indexed-mode))

(use-package vertico-mouse
  :straight nil
  :init (vertico-mouse-mode))

(use-package vertico-repeat
  :straight nil
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (global-set-key "\M-r" #'vertico-repeat)
  )


(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-b" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  ([remap switch-to-buffer]         . consult-buffer)
  ([remap recentf-open-files]       . consult-recent-file)
  ([remap bookmark-jump]            . consult-bookmark)
  ([remap imenu]                    . consult-imenu)

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq  consult-narrow-key "<"
         consult-line-numbers-widen t
         consult-async-min-input 2
         consult-async-refresh-delay  0.15
         consult-async-input-throttle 0.2
         consult-async-input-debounce 0.1)

  (defun consult-ripgrep-project ()
    (interactive)
    (consult-ripgrep (consult--project-root) (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) nil)))

:config
(setq consult-project-root-function
      (lambda ()
        (when-let (project (project-current))
          (car (project-roots project)))))
)

(provide 'init-completion)
