;;; init-dired.el --- Dired and file manager configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Dired and related file management packages
;; including Dirvish for enhanced directory browsing.

;;; Code:

;; Use gls if available (GNU ls for macOS)
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;; Basic Dired configuration
(use-package dired
  :straight nil
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)

  :general
  (my-leader-def "fd" 'dired)
  (:states 'normal :keymaps 'dired-mode-map
           "SPC" nil))

;; Dirvish - Modern file manager
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (dirvish-side-follow-mode)
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  (add-hook 'dirvish-find-entry-hook
            (lambda (&rest _) (setq-local truncate-lines t)))

  ;; Exa preview support
  (when (executable-find "exa")
    (dirvish-define-preview exa (file)
      "Use `exa' to generate directory preview."
      :require ("exa")
      (when (file-directory-p file)
        `(shell . ("exa" "-al" "--color=always" "--icons"
                   "--group-directories-first" ,file))))

    (add-to-list 'dirvish-preview-dispatchers 'exa))

  :bind
  (("C-c f" . dirvish-fd))

  :general
  (:states 'normal :keymaps 'dirvish-mode-map
           "?"    #'dirvish-dispatch
           "q"    #'dirvish-quit
           "a"    #'dirvish-quick-access
           "f"    #'dirvish-file-info-menu
           "y"    #'dirvish-yank-menu
           "N"    #'dirvish-narrow
           "^"    #'dirvish-history-last
           "h"    #'dirvish-history-jump
           "s"    #'dirvish-quicksort
           "v"    #'dirvish-vc-menu
           "TAB"  #'dirvish-subtree-toggle
           "M-f"  #'dirvish-history-go-forward
           "M-b"  #'dirvish-history-go-backward
           "M-l"  #'dirvish-ls-switches-menu
           "M-m"  #'dirvish-mark-menu
           "M-t"  #'dirvish-layout-toggle
           "M-s"  #'dirvish-setup-menu
           "M-e"  #'dirvish-emerge-menu
           "M-j"  #'dirvish-fd-jump
           "<mouse-1>" #'dirvish-subtree-toggle))

;; Dired extensions
(use-package dired-x
  :straight nil
  :config
  ;; Hide dotfiles by default
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Syntax highlighting for Dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; VSCode-style icons
(use-package vscode-icon
  :config
  (push '("jpg" . "image") vscode-icon-file-alist))

;; Side panel for Dirvish
(use-package dirvish-side
  :straight nil
  :custom
  (dirvish-side-width 42)
  :general
  (my-leader-def "fs" #'dirvish-side))

(provide 'init-dired)

;;; init-dired.el ends here
