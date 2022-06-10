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
  :init
  (evil-collection-init))

(use-package evil-surround
  :init (global-evil-surround-mode 1))

(use-package evil-commentary
  :init
  (add-hook 'after-init-hook #'evil-commentary-mode))

(use-package evil-mc
  :init
  (global-evil-mc-mode 1)
  (evil-define-key 'visual evil-mc-key-map (kbd "C-d") 'evil-mc-make-and-goto-next-match)
  )

(use-package evil-search-highlight-persist
  :custom-face
  (evil-search-highlight-persist-highlight-face ((t (:background "maroon"))))
  :init
  (global-evil-search-highlight-persist t)
  )

(provide 'init-evil)
