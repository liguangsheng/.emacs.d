(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

  (my-leader-def
    "fE" #'treemacs
    "fe" #'treemacs-select-window)

  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (progn
    (setq
     treemacs-deferred-git-apply-delay      0.5
     treemacs-directory-name-transformer    #'identity
     treemacs-display-in-side-window        t
     treemacs-eldoc-display                 t
     treemacs-file-event-delay              5000
     treemacs-file-extension-regex          treemacs-last-period-regex-value
     treemacs-file-follow-delay             0.5
     treemacs-file-name-transformer         #'identity
     treemacs-follow-after-init             t
     treemacs-git-command-pipe              ""
     treemacs-goto-tag-strategy             'refetch-index
     treemacs-indentation                   2
     treemacs-indentation-string            " "
     treemacs-is-never-other-window         nil
     treemacs-max-git-entries               5000
     treemacs-missing-project-action        'ask
     treemacs-move-forward-on-expand        nil
     treemacs-no-delete-other-windows       t
     treemacs-no-png-images                 nil
     treemacs-position                      'left
     treemacs-project-follow-cleanup        nil
     treemacs-python-executable             (executable-find "python")
     treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
     treemacs-recenter-after-file-follow    nil
     treemacs-recenter-after-project-expand 'on-distance
     treemacs-recenter-after-project-jump   'always
     treemacs-recenter-after-tag-follow     nil
     treemacs-recenter-distance             0.1
     treemacs-show-cursor                   nil
     treemacs-show-hidden-files             t
     treemacs-silent-filewatch              nil
     treemacs-silent-refresh                nil
     treemacs-sorting                       'alphabetic-asc
     treemacs-space-between-root-nodes      t
     treemacs-width-is-initially-locked     nil
     treemacs-tag-follow-cleanup            t
     treemacs-tag-follow-delay              1.5
     treemacs-user-mode-line-format         nil
     treemacs-width                         40)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 14)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))


  :bind
  (:map global-map
		("<f2>"      . treemacs)
		("M-0"       . treemacs-select-window)
		("C-x t 1"   . treemacs-delete-other-windows)
		("C-x t t"   . treemacs)
		("C-x t B"   . treemacs-bookmark)
		("C-x t C-t" . treemacs-find-file)
		("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'init-treemacs)
