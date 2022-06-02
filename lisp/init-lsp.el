(use-package lsp-mode
  :diminish lsp-mode

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
	lsp-enable-completion-at-point t
	lsp-enable-on-type-formatting nil
	lsp-diagnostics-modeline-scope :project
	lsp-rust-server 'rust-analyzer
	)


  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-modeline-mode)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'lsp-deferred)

  :config
  (define-key lsp-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point)
  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (my-leader-def
    :keymaps 'lsp-mode-map
    "ci" #'lsp-find-implementation
    "cR" #'lsp-rename
    "cA" #'lsp-execute-code-action)
  )

(use-package lsp-treemacs
  :requires (lsp treemacs))

(use-package lsp-ui
  :init
  (setq lsp-ui-sideline-show-diagnostics nil
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-position 'top
	lsp-ui-peek-enable t
	lsp-ui-doc-enable nil
	lsp-ui-imenu-enable t
	lsp-ui-flycheck-enable t
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
	lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
			      ,(face-foreground 'font-lock-string-face)
			      ,(face-foreground 'font-lock-constant-face)
			      ,(face-foreground 'font-lock-variable-name-face))
	)
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (define-key (current-global-map) (kbd "C-c u") #'lsp-ui-imenu)

  :config
  (add-hook 'after-load-theme-hook
	    (lambda ()
	      (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
	      (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
	    (lambda ()
	      (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
	      (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))

  (define-key lsp-ui-mode-map (kbd "M-RET") #'lsp-ui-sideline-apply-code-actions)
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide) ;; `C-g'to close doc
  )


(use-package lsp-pyright
  :init
  (add-hook 'python-mode-hook (lambda () (require 'lsp-pyright) (lsp-deferred))))

(provide 'init-lsp)
