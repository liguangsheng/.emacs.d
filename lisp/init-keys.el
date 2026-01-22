;;; init-keys.el --- Keybinding configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures keybindings using general, hydra, and which-key.

;;; Code:

;; Hydra packages for modal interfaces
(use-package hydra)

(use-package major-mode-hydra
  :bind ("M-SPC" . major-mode-hydra))

(use-package pretty-hydra
  :demand t
  :preface
  (defun config-hydras--add-quit-bindings (result)
    "Add quit bindings to hydra heads."
    (append '(("q" nil :exit t)
              ("<escape>" nil :exit t))
            result))
  :config
  (advice-add #'pretty-hydra--get-heads :filter-return #'config-hydras--add-quit-bindings))

;; Which-key for keybinding hints
(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-idle-delay 0.4
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-side-window-max-height 0.25
        which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-setup-minibuffer)
  (which-key-mode 1))

;; General for leader keybindings
(use-package general
  :init
  (defvar my-leader-key "SPC"
    "Leader key for global commands.")

  (defvar my-leader-alt-key "C-,"
    "Alternative leader key.")

  (defvar my-local-leader-key "SPC m"
    "Local leader key for mode-specific commands.")

  (defvar my-local-leader-alt-key "C-, m"
    "Alternative local leader key.")

  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :prefix my-leader-key
    :non-normal-prefix my-leader-alt-key
    :global-prefix my-leader-alt-key)

  (general-create-definer my-local-leader-def
    :states '(normal insert visual emacs)
    :prefix my-local-leader-key
    :non-normal-prefix my-local-leader-alt-key
    :global-prefix my-local-leader-alt-key)

  :config
  (my-leader-def
    ;; General commands
    "SPC" '(avy-goto-word-1                    :wk "Find word")
    "RET" '(bookmark-jump                      :wk "Jump to bookmark")
    ":"   '(execute-extended-command           :wk "M-x")
    ";"   '(eval-last-sexp                     :wk "Eval last sexp")
    "`"   '(evil-switch-to-windows-last-buffer :wk "Switch to last buffer")
    "="   'my-format-buffer

    ;; Code navigation
    "c"   '(:ignore t :wk "code")
    "cd"  'xref-find-definitions
    "co"  'xref-find-definitions-other-window
    "cr"  'xref-find-references
    "cI"  'imenu

    ;; File operations
    "f"   '(:ignore t :wk "file")
    "fI"  '(open-init-el           :wk "Open ~/.emacs.d/init.el")
    "ff"  '(find-file              :wk "Find file")
    "fd"  '(find-dir               :wk "Find directory")
    "fr"  '(recentf-open-files     :wk "Recent files")
    "fF"  '(my/find-file-from-here :wk "Find file from here")

    ;; Quit commands
    "q"   '(:ignore t :wk "quit")
    "qQ"  '(kill-emacs    :wk "Quit Emacs")
    "qr"  '(restart-emacs :wk "Restart Emacs")))

;;; Global keybindings
(global-set-key (kbd "M-j") #'next-window-any-frame)
(global-set-key (kbd "M-k") #'previous-window-any-frame)
(global-set-key (kbd "M-[") #'xref-pop-marker-stack)
(global-set-key (kbd "M-]") #'xref-find-definitions)
(global-set-key (kbd "C-c C-f") #'my-format-buffer)

(provide 'init-keys)

;;; init-keys.el ends here
