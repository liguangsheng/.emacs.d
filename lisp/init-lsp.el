;;; init-lsp.el --- Language Server Protocol configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures LSP (Language Server Protocol) support including
;; lsp-mode, lsp-ui, and language-specific server configurations.

;;; Code:

;; Core LSP mode configuration
(use-package lsp-mode
  :diminish

  :custom
  (lsp-auto-guess-root t)                   ; Detect project root
  (lsp-diagnostics-modeline-scope :project)
  (lsp-enable-completion-at-point t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil)
  (lsp-go-use-gofumpt t)
  (lsp-inhibit-message t)
  (lsp-keep-workspace-alive nil)
  (lsp-message-project-root-warning t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-prefer-flymake nil)                  ; Use lsp-ui and flycheck
  (lsp-signature-auto-activate nil)
  (lsp-rust-server 'rust-analyzer)
  (lsp-headerline-breadcrumb-segments '(project file symbols))

  :init
  ;; Keybindings in lsp-mode
  (add-hook 'lsp-mode-hook
            (lambda ()
              (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
              (define-key lsp-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point)
              (define-key lsp-mode-map (kbd "C-<down-mouse-1>") #'lsp-find-definition-mouse)
              (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
              (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)

              (general-define-key
               :states 'normal :keymaps 'local
               "gd" #'lsp-find-definition
               "gr" #'lsp-find-references
               "gi" #'lsp-find-implementation
               "K"  #'lsp-describe-thing-at-point)))

  ;; Helper function to enable LSP with formatting
  (defun lsp-turn-on-hook ()
    (lsp-deferred)
    (my-leader-def :keymaps 'local "=" #'lsp-format-buffer))

  ;; Enable LSP for specific modes
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-modeline-mode)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'lsp-turn-on-hook)

  :pretty-hydra
  ((:color teal :quit-key "q" :title "LSP: language server protocol")
   ("Server"
    (("s" lsp "toggle" :toggle lsp-mode)))
   "Navigation"
   (("r" lsp-find-references "references")
    ("d" lsp-find-definition "definition")
    ("i" lsp-find-implementation "implementation")
    ("e" lsp-treemacs-errors-list "errors list"))
   "Refactor"
   (("R" lsp-rename "rename")
    ("f" lsp-format-buffer "format")
    ("a" lsp-execute-code-action "code action"))
   "Diagnostics"
   (("n" flymake-goto-next-error "next error")
    ("p" flymake-goto-prev-error "prev error"))
   "Workspace"
   (("S" lsp-workspace-restart "restart")
    ("c" lsp-workspace-configuration "configuration"))
   "Help"
   (("h" lsp-describe-thing-at-point)))

  :general
  (my-leader-def
   "l" #'lsp-mode-hydra/body))

;; LSP UI enhancements
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
        lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (define-key (current-global-map) (kbd "C-c u") #'lsp-ui-imenu)

  :config
  ;; Update colors after theme change
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
              (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))

  (define-key lsp-ui-mode-map (kbd "M-RET") #'lsp-ui-sideline-apply-code-actions)
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)) ;; C-g to close doc

;; LSP Treemacs integration
(use-package lsp-treemacs
  :requires (lsp treemacs))

;; Python LSP server
(use-package lsp-pyright
  :init
  (add-hook 'python-mode-hook (lambda () (require 'lsp-pyright) (lsp-deferred))))

(provide 'init-lsp)

;;; init-lsp.el ends here
