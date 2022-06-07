(use-package lsp-bridge
  :straight '(lsp-bridge
	      :type git
	      :host github
	      :repo "manateelazycat/lsp-bridge"
	      :files (:defaults "*"))

  :init
  (setq lsp-bridge-enable-log t)

  (defun my/lsp-bridge-find-def ()
    (interactive)
    (let ((prev-marker (point-marker)))
      (lsp-bridge-find-def)
      (xref-push-marker-stack prev-marker)))

  ;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
  ;; (defun lsp-bridge-mix-multi-backends ()
  ;;   (setq-local completion-category-defaults nil)
  ;;   (setq-local completion-at-point-functions
  ;; 		(list
  ;; 		 (cape-capf-buster
  ;;                 (cape-super-capf
  ;;                  #'lsp-bridge-capf
  ;;                  #'cape-keyword
  ;;                  #'cape-file
  ;;                  #'cape-dabbrev
  ;;                  )
  ;;                 'equal)
  ;; 		 )))

  ;; Enable auto completion in elisp mode.
  ;; (add-hooks 'emacs-lisp-mode-hook
  ;; 	     (lambda () (setq-local corfu-auto t)))

  (require 'lsp-bridge)             ;; load lsp-bridge

  ;; Enable lsp-bridge.
  (add-hooks (list
	      'emacs-lisp-mode
	      'c-mode-hook
	      'c++-mode-hook
	      'python-mode-hook
	      'rust-mode-hook
	      'go-mode-hook
	      'haskell-mode-hook
	      'js2-mode-hook
	      'js-mode-hook
	      'sh-mode-hook
	      'web-mode-hook
	      )
	     (lambda ()
	       (with-eval-after-load 'corfu (corfu-mode -1))
	       (lsp-bridge-mode 1)
	       (my-leader-def :keymaps 'local
		 "c i" 'lsp-bridge-find-impl
		 "c R" 'lsp-bridge-rename
		 )
	       (local-set-key [remap xref-find-definitions] #'my/lsp-bridge-find-def)
	       (local-set-key [remap xref-find-definitions-other-window] #'lsp-bridge-find-def-other-window)
	       (local-set-key [remap xref-find-references]  #'lsp-bridge-find-references)))

  (evil-set-initial-state 'lsp-bridge-ref-mode 'insert)
  )

(provide 'init-lsp-bridge)
