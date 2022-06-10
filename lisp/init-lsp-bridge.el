(use-package lsp-bridge
  :straight '(lsp-bridge
	      :type git
	      :host github
	      :repo "manateelazycat/lsp-bridge"
	      :files (:defaults "*"))

  :init
  (defun my/lsp-bridge-find-def ()
    (interactive)
    (let ((prev-marker (point-marker)))
      (lsp-bridge-find-def)
      (xref-push-marker-stack prev-marker)))

  (defun lsp-bridge-mode-hook-func ()
    (with-eval-after-load 'corfu (corfu-mode -1))

    (my-leader-def :keymaps 'local
      "="   'lsp-bridge-code-format
      "c i" 'lsp-bridge-find-impl
      "c R" 'lsp-bridge-rename
      "c a" 'lsp-bridge-code-action
      )

    (local-set-key (kbd "M-[")                                #'lsp-bridge-return-from-def)
    (local-set-key (kbd "M-]")                                #'lsp-bridge-find-def)
    (local-set-key [remap xref-find-definitions]              #'lsp-bridge-find-def)
    (local-set-key [remap xref-find-definitions-other-window] #'lsp-bridge-find-def-other-window)
    (local-set-key [remap xref-find-references]               #'lsp-bridge-find-references)

    (evil-define-key 'normal 'local (kbd "gd")  #'lsp-bridge-find-def)
    (evil-define-key 'normal 'local (kbd "gD")  #'lsp-bridge-find-def-other-window)
    (evil-define-key 'normal 'local (kbd "gi")  #'lsp-bridge-find-impl)
    (evil-define-key 'normal 'local (kbd "gI")  #'lsp-bridge-find-impl-other-window)
    (evil-define-key 'normal 'local (kbd "gr")  #'lsp-bridge-find-references)
    (evil-define-key 'normal 'local (kbd "K")   #'lsp-bridge-popup-documentation)
    (evil-define-key 'normal 'local (kbd "[d")  #'lsp-bridge-diagnostic-jump-prev)
    (evil-define-key 'normal 'local (kbd "]d")  #'lsp-bridge-diagnostic-jump-next)
    )

  (setq lsp-bridge-enable-log   nil
	acm-enable-icon         t
	acm-enable-doc          t
	acm-enable-tabnine      nil
	acm-enable-quick-access t)
  (require 'lsp-bridge)
  (evil-set-initial-state 'lsp-bridge-ref-mode 'insert)
  (add-hook 'lsp-bridge-mode-hook #'lsp-bridge-mode-hook-func)

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
	      'lua-mode-hook
	      )
	     #'lsp-bridge-mode
	     )
  )

(provide 'init-lsp-bridge)
