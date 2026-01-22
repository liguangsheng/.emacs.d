;;; init-evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;; Author: lgs
;; Keywords: evil, vim, emacs

;;; Commentary:
;; This file configures Evil mode, a Vim emulation layer for Emacs,
;; along with related packages for enhanced editing capabilities.

;;; Code:

;; Core Evil mode configuration
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-tree)
  (evil-mode 1)

  :config
  ;; Custom ex commands
  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit)
  ;; Optional: Set initial state for special-mode
  ;; (evil-set-initial-state 'special-mode 'insert)
  )

;; Evil collection for better integration with Emacs modes
(use-package evil-collection
  :init
  (evil-collection-init))

;; Evil surround for surrounding text objects
(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

;; Evil commentary for commenting
(use-package evil-commentary
  :init
  (add-hook 'after-init-hook #'evil-commentary-mode))

;; Evil multiple cursors
(use-package evil-mc
  :init
  (global-evil-mc-mode 1)
  (evil-define-key 'visual evil-mc-key-map
    (kbd "C-d") 'evil-mc-make-and-goto-next-match))

;; Persistent search highlighting
(use-package evil-search-highlight-persist
  :custom-face
  (evil-search-highlight-persist-highlight-face ((t (:background "maroon"))))
  :init
  (global-evil-search-highlight-persist t))

(provide 'init-evil)

;;; init-evil.el ends here
