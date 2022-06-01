(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding  nil
	evil-undo-system      'undo-tree)
  (evil-mode 1)

  :config
  (evil-ex-define-cmd "W"    'evil-write)
  (evil-ex-define-cmd "q"    'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :hook (after-init . evil-commentary-mode))

(provide 'init-evil)
