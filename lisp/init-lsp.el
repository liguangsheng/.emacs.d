(use-package lsp-mode
  :diminish lsp-mode
    :hook ((lsp-mode . (lambda ()
		       ;; Integrate `which-key'
		       (lsp-enable-which-key-integration)

		       ;; Format and organize imports
		       ;;  (add-h ook 'before-save-hook #'lsp-format-buffer t t)
		       ;;  (add-hook 'before-save-hook #'lsp-organize-imports t t))
		       ))
	 (lsp-managed-mode-hook . 'lsp-diagnostics-modeline-mode)
	 )

  :bind (:map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point)
	      ([remap xref-find-definitions] . lsp-find-definition)
	      ([remap xref-find-references] . lsp-find-references))

  :init
  (setq lsp-auto-guess-root t       ; Detect project root
	lsp-inhibit-message t
	lsp-message-project-root-warning t
	lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
	lsp-keep-workspace-alive nil
	lsp-signature-auto-activate nil
	lsp-modeline-code-actions-enable nil
	lsp-modeline-diagnostics-enable nil
	lsp-modeline-workspace-status-enable nil
	lsp-enable-file-watchers nil
	lsp-enable-folding nil
	lsp-enable-symbol-highlighting t
	lsp-enable-text-document-color nil
	lsp-enable-indentation nil
	lsp-enable-on-type-formatting nil
	lsp-diagnostics-modeline-scope :project
	)
  )
(use-package lsp-treemacs
  :requires (lsp treemacs))

;; (use-package company-lsp
;;   :requires (company lsp)
;;   :init
;;   (setq company-lsp-cache-candidates 'auto)
;;   (push 'company-lsp company-backends))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   ;; :init (setq scroll-margin 10)
;;   :config  (add-hook 'after-load-theme-hook
;; 		     (lambda ()
;; 		       (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
;; 		       (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))
;;   :bind (("C-c u" . lsp-ui-imenu)
;; 	 :map lsp-ui-mode-map
;; 	 ("M-RET" . lsp-ui-sideline-apply-code-actions))
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :init (setq lsp-ui-sideline-show-diagnostics nil
;; 	      lsp-ui-sideline-ignore-duplicate t
;; 	      ;; lsp-ui-doc-position 'at-point
;; 	      lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
;; 	      lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
;; 				    ,(face-foreground 'font-lock-string-face)
;; 				    ,(face-foreground 'font-lock-constant-face)
;; 				    ,(face-foreground 'font-lock-variable-name-face)))
;;   :config
;;   ;; `C-g'to close doc
;;   (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

;;   ;; Reset `lsp-ui-doc-background' after loading theme
;;   (add-hook 'after-load-theme-hook
;; 	    (lambda ()
;; 	      (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
;; 	      (set-face-background 'lsp-ui-doc-background (face-background 'tooltip)))))

(setq lsp-rust-server 'rust-analyzer)

(add-hooks #'lsp-deferred (list 'python-mode-hook
				'go-mode-hook
				'rust-mode-hook
				'lua-mode-hook
				'powershell-mode-hook))

(provide 'init-lsp)
