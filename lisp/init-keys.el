(use-package hydra)

(use-package major-mode-hydra
  :bind ("M-SPC" . major-mode-hydra))

(use-package pretty-hydra
  :demand t
  :preface
  (defun config-hydras--add-quit-bindings (result)
    (append '(("q" nil :exit t)
			  ("<escape>" nil :exit t))
            result))
  :config
  (advice-add #'pretty-hydra--get-heads :filter-return #'config-hydras--add-quit-bindings))

(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
		which-key-side-window-location 'bottom
		which-key-idle-delay 0.4
		which-key-separator " → "
		which-key-prefix-prefix "+"
		which-key-side-window-max-heght 0.25
		which-key-sort-order 'which-key-prefix-then-key-order
		)
  (which-key-setup-minibuffer)
  (which-key-mode 1)
  )

(use-package general
  :init
  (defvar my-leader-key "SPC")
  (defvar my-leader-alt-key "C-,")
  (defvar my-local-leader-key "SPC m")
  (defvar my-local-leader-alt-key "C-, m")

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
    "SPC" '(avy-goto-word-1                    :wk "Find file in project")
    "RET" '(bookmark-jump                      :wk "Jump to bookmark")
    ":"   '(execute-extended-command           :wk "M-x")
    ";"   '(eval-last-sexp                     :wk "Eval last sexp")
    "`"   '(evil-switch-to-windows-last-buffer :wk "Switch to last buffer")
    "="   'my-format-buffer

    ;; code
    "c"   '(:ignore t :wk "code")
    "cd" 'xref-find-definitions
    "co" 'xref-find-definitions-other-window
    "cr" 'xref-find-references
    "cI" 'imenu

    ;; file
    "f"   '(:ignore t :wk "file")
    "fI" '(open-init-el           :wk "open ~/.emacs.d/init.el")
    "ff" '(find-file              :wk "Find file")
    "fd" '(find-dir               :wk "Find file")
    "fr" '(recentf-open-files     :wk "Recent files")
    "fF" '(my/find-file-from-here :wk "Find file from here")

    ;; quit
    "q" '(:ignore t :wk "quit")
    "qQ" '(kill-emacs    :wk "Quit emacs")
    "qr" '(restart-emacs :wk "Restart emacs")
    )
  )

(global-set-key (kbd "M-j") #'next-window-any-frame)
(global-set-key (kbd "M-k") #'previous-window-any-frame)
(global-set-key (kbd "M-[") #'xref-pop-marker-stack)
(global-set-key (kbd "M-]") #'xref-find-definitions)
(global-set-key (kbd "C-c C-f") #'my-format-buffer)

(provide 'init-keys)
