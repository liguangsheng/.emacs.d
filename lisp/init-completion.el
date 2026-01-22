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
  (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)

  (setq completion-styles '(substring orderless))
  )

(use-package vertico-directory
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-indexed
  :init (vertico-indexed-mode))

(use-package vertico-mouse
  :init (vertico-mouse-mode))

(use-package vertico-repeat
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

  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref))

(use-package svg-lib)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu
  :straight '(corfu
			  :files (:defaults "extensions/*")
			  :includes (corfu-history
						 corfu-info
						 corfu-indexed
						 corfu-quick))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-preview-current nil)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  ;; (corfu-min-width 42)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.

  :init
  (global-corfu-mode)
  (corfu-indexed-mode)

  :general
  (:states 'insert :keymaps 'corfu-map)
  )

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package corfu-doc
  :if *gui*
  :init
  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  :config
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
  )

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)

         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  )

;; citre或者lsp-bridge中引用了tabnine的capf
;; (use-package tabnine-capf
;;   :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
;;   :init
;;   (require 'tabnine-capf)

;;   :config
;;   (add-hook 'kill-emacs-hook #'tabnine-capf-kill-process)
;;   )

(provide 'init-completion)
