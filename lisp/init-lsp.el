(use-package lsp-mode
  :diminish lsp-mode

  :init
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-modeline-mode)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'lsp-deferred)

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
	lsp-keymap-prefix "C-l"
	)

  :config
  (define-key lsp-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point)
  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)
  (my-leader-def
    :keymaps 'lsp-mode-map
    "ci" #'lsp-find-implementation
    "cR"  #'lsp-rename)
  )

(use-package lsp-treemacs
  :requires (lsp treemacs))

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

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(provide 'init-lsp)
