(use-package lsp-mode
  :diminish lsp-mode
  :hook (prog-mode . lsp)
  :bind (("s-b" . xref-find-definitions)
	 ("s-]" . xref-find-definitions)
	 ("s-[" . evil-jump-backward))
  :init 
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-diagnostic-functions '(lsp--flymake-backend nil))
  :config
  (setq lsp-inhibit-message t
	lsp-message-project-root-warning t
	create-lockfiles nil
	lsp-session-file (concat emacs-cache-dir "lsp-session"))

  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  (require 'lsp-clients)

  ;; C/C++ Mode
  ;; brew install ccls
  (use-package ccls
    :defines projectile-project-root-files-top-down-recurring
    :hook ((c-mode c++-mode objc-mode cuda-mode) .
	   (lambda () (require 'ccls) (lsp)))
    :init
    (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
    :config
    (with-eval-after-load 'projectile
      (setq projectile-project-root-files-top-down-recurring
	    (append '("compile_commands.json"
		      ".ccls")
		    projectile-project-root-files-top-down-recurring))))

  ;; python-mode
  (add-hook 'python-mode 'lsp)
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))

  ;; go-mode
  ;; Go support for lsp-mode using Sourcegraph's Go Language Server
  ;; go get -u -v github.com/sourcegraph/go-langserver
  (use-package lsp-go
    :hook (go-mode . lsp))

  ;; rust-mode
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  )


(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq scroll-margin 0))

(provide 'init-lsp)
