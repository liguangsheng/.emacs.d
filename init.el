;; init.el --- Emacs configuration -*- lexical-binding: t; -*- ;;
;; Author: Guangsheng Li <ligs.cn@gmail.com>
;; Maintainer: Guangsheng Li <ligs.cn@gmail.com>

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(require 'init-startup (expand-file-name "lisp/init-startup.el" user-emacs-directory))
(require 'init-packages)

;;; Quick Settings:

(setq-default
 perferences/theme (cond (*windows* (if *gui* 'doom-nord 'base16-nord))
						 (*macos*   (if *gui* 'doom-nord 'doom-nord))
						 (*linux*   (if *gui* 'doom-nord 'doom-one)))

 perferences/font   "Roboto Mono:pixelsize=12"
 perferences/cnfont (font-spec :family "WenQuanYi Micro Hei" :size 14)

 perferences/python-executable (cond (*windows* "C:\\Program Files\\Python39\\python.exe")
									 (t         "python3"))

 perferences/enable-icons    *gui*
 perferences/enable-tabnine  t
 perferences/enable-posframe t
 perferences/enable-server   t

 line-spacing       0.1
 )

(require 'init-basic)

;; Initialize UI
(require 'init-fonts)
(require 'init-theme)
(require 'init-icons)
(require 'init-posframe)

(require 'init-features)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
		evil-want-keybinding nil)
  (evil-mode 1)
  ;; :hook ((text-mode prog-mode fundamental-mode) . #'evil-mode)
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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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
     treemacs-file-follow-delay             0.2
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
     treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
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
     treemacs-tag-follow-cleanup            t
     treemacs-tag-follow-delay              1.5
     treemacs-user-mode-line-format         nil
     treemacs-width                         35)
    
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

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

;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after treemacs projectil 
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

;; (use-package treemacs-persp
;;   :after treemacs persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package wgrep)

(use-package counsel
  :init (setq ivy-height 30
			  ivy-initial-inputs-alist nil)
  :config
  (defun counsel-rg-dir ()
    "在指定文件夹下进行搜索，先到dired选择文件夹，运行此函数"
    (interactive)
    (counsel-rg "" (dired-file-name-at-point)))
  )

(use-package smex)

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package ivy-hydra  :after (:all ivy hydra))

(require 'init-dired)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; (withf 'shut-up (shut-up (yas-global-mode 1))))
  (yas-global-mode 1))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
		company-tooltip-limit 24            ; bigger popup window
		company-idle-delay 0.0              ; decrease delay before autocompletion popup shows
		company-echo-delay 0                ; remove annoying blinking
		company-minimum-prefix-length 1
		company-require-match nil
		company-dabbrev-ignore-case nil
		company-dabbrev-downcase nil)
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

(use-package company-tabnine)

(use-package projectile
  :bind (:map projectile-mode-map
			  ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
			  ("C-c p" . projectile-command-map))

  :hook (after-init . projectile-mode)

  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)

  :config
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
			  (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  (when *windows*
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching t))

    (setq projectile-git-submodule-command nil)

    ;; Support Perforce project
    (let ((val (or (getenv "P4CONFIG") ".p4config")))
      (add-to-list 'projectile-project-root-files-bottom-up val)))
  )

(use-package counsel-projectile
  :after (projectile counsel))

(use-package flycheck)

(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
		which-key-side-window-location 'bottom
		which-key-idle-delay 0.4
		which-key-separator " → "
		which-key-prefix-prefix "+"
		which-key-side-window-max-heght 0.25)
  :config
  (which-key-mode 1))

(use-package lsp-mode
  :diminish lsp-mode
  :custom-face
  (lsp-headerline-breadcrumb-path-error-face
   ((t :underline (:style line :color ,(face-foreground 'error))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-warning-face
   ((t :underline (:style line :color ,(face-foreground 'warning))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-info-face
   ((t :underline (:style line :color ,(face-foreground 'success))
       :inherit lsp-headerline-breadcrumb-path-face)))
  (lsp-headerline-breadcrumb-path-hint-face
   ((t :underline (:style line :color ,(face-foreground 'success))
       :inherit lsp-headerline-breadcrumb-path-face)))

  (lsp-headerline-breadcrumb-symbols-error-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style line :color ,(face-foreground 'error)))))
  (lsp-headerline-breadcrumb-symbols-warning-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style line :color ,(face-foreground 'warning)))))
  (lsp-headerline-breadcrumb-symbols-info-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style line :color ,(face-foreground 'success)))))
  (lsp-headerline-breadcrumb-symbols-hint-face
   ((t :inherit lsp-headerline-breadcrumb-symbols-face
       :underline (:style line :color ,(face-foreground 'success)))))

  :hook ((lsp-mode . (lambda ()
					   ;; Integrate `which-key'
					   (lsp-enable-which-key-integration)

					   ;; Format and organize imports
					   ;;  (add-h ook 'before-save-hook #'lsp-format-buffer t t)
					   ;;  (add-hook 'before-save-hook #'lsp-organize-imports t t))
					   ))
		 (lsp-managed-mode-hook . 'lsp-diagnostics-modeline-mode))

  :bind (:map lsp-mode-map
			  ("C-c C-d" . lsp-describe-thing-at-point)
			  ([remap xref-find-definitions] . lsp-find-definition)
			  ([remap xref-find-references] . lsp-find-references))

  :bind (("M-b" . xref-find-definitions)
		 ("M-]" . xref-find-definitions)
		 ("M-[" . xref-pop-marker-stack))

  :init
  (setq lsp-auto-guess-root t       ; Detect project root
		lsp-inhibit-message t
		lsp-message-project-root-warning t
		lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
		lsp-keep-workspace-alive nil
		lsp-signature-auto-activate nil
		lsp-modeline-code-actions-enable nil
		lsp-modeline-diagnostics-enable nil
		lsp-modeline-workspace-status-enable nil
		lsp-enable-file-watchers nil
		lsp-enable-folding nil
		lsp-enable-symbol-highlighting nil
		lsp-enable-text-document-color nil
		lsp-enable-indentation nil
		lsp-enable-on-type-formatting nil
		lsp-diagnostics-modeline-scope :project
		)

  :config
  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted.")))

(with-eval-after-load 'treemacs
  (use-package lsp-treemacs))

;; (use-package company-lsp
;;   :init (setq company-lsp-cache-candidates 'auto)
;;   (push 'company-lsp company-backends)
;;   )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq scroll-margin 0)
  :config  (add-hook 'after-load-theme-hook
					 (lambda ()
					   (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
					   (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))
  :bind (("C-c u" . lsp-ui-imenu)
		 :map lsp-ui-mode-map
		 ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-sideline-show-diagnostics nil
			  lsp-ui-sideline-ignore-duplicate t
			  ;; lsp-ui-doc-position 'at-point
			  lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
			  lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
									,(face-foreground 'font-lock-string-face)
									,(face-foreground 'font-lock-constant-face)
									,(face-foreground 'font-lock-variable-name-face)))
  :config
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
			(lambda ()
			  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
			  (set-face-background 'lsp-ui-doc-background (face-background 'tooltip)))))

(use-package lsp-ivy)

(require 'init-keybindings)
(require 'init-lang)


;;; Experimental:
(use-package esup)

;;; Load custom file:
(when (and custom-file (file-readable-p custom-file) (load custom-file)))

;;; init.el ends here
