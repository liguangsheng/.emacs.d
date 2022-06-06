(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding  nil
	evil-undo-system      'undo-tree)
  (evil-mode 1)

  :config
  (evil-ex-define-cmd "W"    'evil-write)
  (evil-ex-define-cmd "q"    'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit)
  ;; (evil-set-initial-state 'special-mode 'insert)
  )

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :init (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :init
  (add-hook 'after-init-hook #'evil-commentary-mode))

(provide 'init-evil)
