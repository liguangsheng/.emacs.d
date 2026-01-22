;;; init-features.el --- Additional features and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures various Emacs features and utilities including
;; undo management, syntax highlighting, code formatting, and session management.

;;; Code:

;; Utility packages
(use-package s)
(use-package dash)
(use-package shut-up)
(use-package restart-emacs)
(use-package posframe)

;; Undo tree for better undo/redo
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil
        undo-tree-enable-undo-in-region nil))

;; Window configuration undo/redo
(use-package winner
  :config (winner-mode 1))

;; Text scaling
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease default-text-scale-reset)
  :custom ((default-text-scale-amount 5)))

;; Smooth scrolling for GUI Emacs 28+
(when (and *gui* *emacs28+*)
  (use-package good-scroll
    :config
    (good-scroll-mode 1)))

;; Expand region for smart selection
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Rainbow delimiters for better bracket matching
(use-package rainbow-delimiters
  :hook ((text-mode prog-mode fundamental-mode) . #'rainbow-delimiters-mode))

;; Highlight indentation (disabled)
(use-package highlight-indentation
  :disabled
  :hook ((text-mode prog-mode fundamental-mode) . #'highlight-indentation-current-column-mode))

;; Highlight numbers
(use-package highlight-numbers
  :hook ((text-mode prog-mode fundamental-mode) . #'highlight-numbers-mode))

;; Highlight TODO keywords
(use-package hl-todo
  :init (global-hl-todo-mode))

;; Volatile highlights for operations
(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; Persistent scratch buffer (disabled)
(use-package persistent-scratch
  :disabled
  :init
  (setq persistent-scratch-autosave-interval 60)
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

;; Sync exec path from shell on macOS
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; Vi-style tilde fringe
(use-package vi-tilde-fringe
  :init
  (global-vi-tilde-fringe-mode))

;; Symbol overlay for symbol highlighting
(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all)
         :map symbol-overlay-map
         ([M-up] . symbol-overlay-jump-prev)
         ([M-down] . symbol-overlay-jump-next)))

;; Rainbow mode for color preview (manual activation)
(use-package rainbow-mode
  :defer t
  :commands (rainbow-mode))

;; Yasnippet for code snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; (withf 'shut-up (shut-up (yas-global-mode 1))))
  (yas-global-mode 1))

;; Format all for code formatting
(use-package format-all
  :bind ("C-c C-f" . format-all-buffer)
  :commands (define-format-all-formatter)
  :init
  (setq format-all-formatters '(("Go" goimports)
                                ("Shell" shfmt)
                                ("GraphQL" prettier)
                                ("Protocol Buffer" my-format-buffer))))

;; Desktop session management
(when my-auto-restore-session
  (use-package desktop
    :init
    ;; Delay desktop-read to after-init-hook for automatic session restoration
    (add-hook 'after-init-hook 'desktop-read)

    :config
    ;; Configure desktop save directory and filename
    (setq desktop-dirname             (expand-user-var "desktop")
          desktop-base-file-name      "emacs.desktop"
          desktop-base-lock-name      "emacs.desktop.lock"
          desktop-path                (list desktop-dirname)
          desktop-save                t
          desktop-files-not-to-save   "^$" ; filter out files not to save
          desktop-load-locked-desktop nil
          desktop-auto-save-timeout   30)

    (add-hook 'kill-emacs-hook 'desktop-save-in-desktop-dir) ; save on exit

    :hook
    (emacs-startup . desktop-save-mode)))

;; ANSI color support for log files
(use-package ansi-color
  :config
  (defun display-ansi-color ()
    "Apply ANSI color codes to the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

  (add-hook 'find-file-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match-p "\\.log\\'" buffer-file-name))
                (display-ansi-color)))))

(provide 'init-features)

;;; init-features.el ends here
