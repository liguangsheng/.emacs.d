(use-package lsp-bridge
  :straight '(lsp-bridge
	      :type git
	      :host github
	      :repo "manateelazycat/lsp-bridge"
	      :files (:defaults "*.py"))

  :init
  (require 'lsp-bridge)             ;; load lsp-bridge
  (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
  (require 'lsp-bridge-icon)        ;; show icon for completion items, optional

  (defun my/lsp-bridge-find-def ()
    (interactive)
    (let ((prev-marker (point-marker)))
      (lsp-bridge-find-def)
      (xref-push-marker-stack prev-marker)))

  ;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
  (defun lsp-bridge-mix-multi-backends ()
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions
		(list
		 (cape-capf-buster
                  (cape-super-capf
                   #'lsp-bridge-capf
                   #'cape-file
                   #'cape-dabbrev
                   )
                  'equal)
		 )))

  ;; Enable auto completion in elisp mode.
  (add-hooks 'emacs-lisp-mode-hook
	     (lambda () (setq-local corfu-auto t)))

  ;; Enable lsp-bridge.
  (add-hooks (list
	      'c-mode-hook
	      'c++-mode-hook
	      'java-mode-hook
	      'python-mode-hook
	      'ruby-mode-hook
	      'rust-mode-hook
	      'elixir-mode-hook
	      'go-mode-hook
	      'haskell-mode-hook
	      'haskell-literate-mode-hook
	      'dart-mode-hook
	      'scala-mode-hook
	      'typescript-mode-hook
	      'typescript-tsx-mode-hook
	      'js2-mode-hook
	      'js-mode-hook
	      'rjsx-mode-hook
	      'tuareg-mode-hook
	      'latex-mode-hook
	      'Tex-latex-mode-hook
	      'texmode-hook
	      'context-mode-hook
	      'texinfo-mode-hook
	      'bibtex-mode-hook
	      'clojure-mode-hook
	      'clojurec-mode-hook
	      'clojurescript-mode-hook
	      'clojurex-mode-hook
	      'sh-mode-hook
	      'web-mode-hook
	      )
	     (lambda ()
		     (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
		     (lsp-bridge-mode 1)
		     (lsp-bridge-mix-multi-backends)
		     (my-leader-def :keymaps 'local
		       "c i" 'lsp-bridge-find-impl
		       "c R" 'lsp-bridge-rename
		       )
		     (local-set-key [remap xref-find-definitions] #'my/lsp-bridge-find-def)
		     (local-set-key [remap xref-find-definitions-other-window] #'lsp-bridge-find-def-other-window)
		     (local-set-key [remap xref-find-references]  #'lsp-bridge-find-references)))

  (evil-set-initial-state 'lsp-bridge-ref-mode 'insert))

(provide 'init-lsp-bridge)
