;; init.el --- Emacs configuration -*- lexical-binding: t; -*- ;;
;; Author: Guangsheng Li <ligs.cn@gmail.com>
;; Maintainer: Guangsheng Li <ligs.cn@gmail.com>

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(defconst user-emacs-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Path to .emacs.d/lisp directory where init files exists.")

(defconst user-emacs-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Path to .emacs.d/site-lisp directory.")

;; Add dir to load-path
(add-to-list 'load-path user-emacs-lisp-directory)
(add-to-list 'load-path user-emacs-site-lisp-directory)

;; Recursive add site-lisp to load-path
(let ((default-directory user-emacs-site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))

(require 'subr-x)
(require 'cl-lib)

;; OS Environment see http://ergoemacs.org/emacs_manual/elisp/System-Environment.html
(setq *mac* (eq system-type 'darwin)
      *win64* (eq system-type 'windows-nt)
      *cygwin* (eq system-type 'cygwin)
      *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux))
      *emacs26* (>= emacs-major-version 26)
      *emacs27* (>= emacs-major-version 27))

(defun windows-total-physical-memory ()
  (/ (float (string-to-number 
	     (nth 1 (split-string (shell-command-to-string "wmic computersystem get TotalPhysicalMemory") "\n"))))
     1024 1024 1024))

(setq total-physical-memory
      (cond (*win64* (windows-total-physical-memory))))

(setq *no-money-and-cry* (< total-physical-memory 32)
      *i-am-rich* (> total-physical-memory 32))

;;; Quick Settings:

(setq-default
 ;; Proxy
 ;; url-proxy-services '(("http"  . "127.0.0.1:1080")
 ;; 		         ("https" . "127.0.0.1:1080")))
 use-tabnine *i-am-rich*
 ;; 尽量使用posframe
 prefer-posframe t
 ;; 尽量使用图标
 prefer-icons (display-graphic-p)
 ;; org files directory
 org-directory "~/sync/org"
 ;; python 可执行文件地址
 python-shell-interpreter "python3"
 )

(when *win64*
  (setq org-directory "e:/sync/org/"
	python-shell-interpreter "C:\\Program Files\\Python39\\python.exe"
	))

;;; Better Defaults:

(when (< emacs-major-version 27)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Better variables
(setq
 apropos                                  t
 backup-by-copying                        t
 comint-prompt-read-only                  t
 compilation-always-kill                  t
 compilation-ask-about-save               nil
 compilation-scroll-output                t
 create-lockfiles                         nil
 custom-file                              "~/custom.el"
 debug-on-error                           t
 delete-old-versions                      t
 font-lock-maximum-size                   5000000
 history-length                           1024
 idle-update-delay                        0.5
 inhibit-startup-message                  t
 kept-new-versions                        6
 kept-old-versions                        2
 large-file-warning-threshold             100000000
 tab-width                                4
 vc-follow-symlinks                       t
 version-control                          t
 visible-bell                             0
 make-backup-files                        nil
 inhibit-compacting-font-caches           t
 )

(defalias 'yes-or-no-p 'y-or-n-p)
;; (cua-mode 1)
(horizontal-scroll-bar-mode -1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(ignore-errors (savehist-mode 1))
(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; 高亮当前行
(global-hl-line-mode 1)

;;; Line Number:
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; 全局开启折行模式
(set-default 'truncate-lines t)
(setq-default truncate-partial-width-windows nil)
(global-visual-line-mode t)

;; Use utf-8 as default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))               ; pretty
(prefer-coding-system                   'utf-8)  ; pretty
(set-terminal-coding-system             'utf-8)  ; pretty
(set-keyboard-coding-system             'utf-8)  ; pretty
(set-selection-coding-system            'utf-8)  ; pretty
(setq locale-coding-system              'utf-8)  ; pretty
(setq-default buffer-file-coding-system 'utf-8)  ; with sugar on top

;; Chinese encoding for windows
(when (eq system-type 'windows-nt)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

;;; Packages:

(require 'package)
(setq package-check-signature nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
	auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

(use-package gcmh
  :ensure t
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000) ;; 100 MB
  :hook (after-init . gcmh-mode))


;; Features
(use-package s)

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(setq-default keep-alive-buffers '("\\**\\*"
				   "^\\*scratch\\*$"
				   "^\\*Messages\\*$"
				   ))

(defun kill-all-buffers ()
  (interactive)
  (cl-loop for buffer in (buffer-list)
	   for bufname = (s-trim (buffer-name buffer))
	   unless (cl-loop for rx in keep-alive-buffers
			   when (> (s-count-matches rx bufname) 0)
			   return bufname)
	   do (kill-buffer bufname)))

(defun kill-other-buffers ()
  (interactive)
  (cl-loop for buffer in (buffer-list)
	   for bufname = (buffer-name buffer)
	   unless (or (string= bufname (buffer-name (current-buffer)))
		      (cl-loop for rx in keep-alive-buffers
			       when (> (s-count-matches rx bufname) 0)
			       return bufname))
	   do (progn (print bufname) (kill-buffer bufname))))

(defun switch-to-modified-buffer ()
  "Switch to modified buffer"
  (interactive)
  (let ((buf-list
	 (seq-filter
	  (lambda (x) (not (or
			    (not (buffer-modified-p x))
			    (s-prefix? "*" (buffer-name x))
			    (s-prefix? " *" (buffer-name x))
			    (s-suffix? "-mode" (buffer-name x)))))
	  (buffer-list))))
    (if buf-list
	(switch-to-buffer (first buf-list))
      (message "No modified buffer."))))

(defun open-init-el ()
  "Open ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-inbox ()
  "Open ~/INBOX"
  (interactive)
  (find-file "~/INBOX"))

(defun switch-to-scratch ()
  "Swtich to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun random-choice (LIST)
  "Get a random choice from LIST"
  (nth (mod (random) (length LIST)) LIST))

(defmacro withf (func &rest body)
  (declare (indent 1) (debug t))
  `(when (fboundp ,func) ,@body))

(use-package all-the-icons :if prefer-icons)

(use-package evil
  :hook ((text-mode prog-mode fundamental-mode) . #'evil-mode)
  :config
  (evil-ex-define-cmd "W"    'evil-write)
  (evil-ex-define-cmd "q"    'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit))

;; (use-package evil-leader
;;   :config
;;   (global-evil-leader-mode))

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

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil
        undo-tree-enable-undo-in-region nil))

(use-package dash)
(use-package shut-up)
(use-package winner
  :config (winner-mode 1))

(use-package restart-emacs)

(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease default-text-scale-reset)
  :custom ((default-text-scale-amount 5)))

;; 平滑滚动屏幕
(use-package good-scroll
  :config
  (good-scroll-mode 1))

;; 这个feature可能会影响company的候选框的显示
(use-package fill-column-indicator
  :disabled 
  :init (setq fci-rule-column 120)
  (add-hook 'prog-mode-hook #'fci-mode))

;; 扩展选择区域
(use-package expand-region)

;; 跳转
(use-package avy
  :init
  (avy-setup-default))

;; emoji
(when (display-graphic-p)
  (use-package emojify
    :hook (after-init . global-emojify-mode)))

;; 彩虹分隔符
(use-package rainbow-delimiters
  :hook ((text-mode prog-mode fundamental-mode) . #'rainbow-delimiters-mode))

;; 高亮缩进
(use-package highlight-indentation
  :disabled
  :hook ((text-mode prog-mode fundamental-mode) . #'highlight-indentation-current-column-mode))

;; 高亮数字
(use-package highlight-numbers
  :hook ((text-mode prog-mode fundamental-mode) . #'highlight-numbers-mode))

;; 高亮TODO
(use-package hl-todo
  :init (global-hl-todo-mode))

(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; 高亮对应的paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
	      show-paren-when-point-in-periphery t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package vi-tilde-fringe
  :init
  (global-vi-tilde-fringe-mode))

(use-package symbol-overlay
  :diminish
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  )

;; 将buffer中#000000样式的16进制rgb渲染出染色
;; 需要的时候手动开启M-x rainbow-mode
(use-package rainbow-mode
  :defer t
  :commands (rainbow-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
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
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
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

(use-package counsel
  :init (setq ivy-height 30
	      ivy-initial-inputs-alist nil))

(use-package smex)

(use-package all-the-icons-ivy
  :if prefer-icons
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :if prefer-icons
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dired-hacks-utils)

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
  :init
  (setq projectile-indexing-method 'native
	projectile-enable-caching nil)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  (setq create-lockfiles nil
	lsp-auto-guess-root t       ; Detect project root
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

;;; Keybindings
(with-eval-after-load 'hydra
  (pretty-hydra-define hydra-launcher (:color teal :title "Overview")
    ("Groups"
     (("b"    hydra-buffers/body   "+ buffers")
      ("c"    hydra-comments/body  "+ comment")
      ("f"    hydra-files/body     "+ files")
      ("p"    hydra-projects/body  "+ project")
      ("T"    hydra-toggles/body   "+ toggles")
      ("w"    hydra-windows/body   "+ windows")
      ("m"    major-mode-hydra     "+ major-mode")
      ("l"    hydra-lsp/body       "+ lsp")
      ("e"    hydra-motions/body   "+ motions"))

     "Actions"
     (("Qq" save-buffers-kill-emacs "quit emacs" :exit t)
      ("Qr" restart-emacs "restart emacs" :exit t)
      ("!"  shell-command "run shell command")
      (":"  eval-expression "eval lisp expression")
      ("d"  dired "dired")
      ("D"  dired-other-window "dired(other window)")
      ("t"  treemacs)
      ("E"  er/expand-region "expand region"))

     "Others"
     (("z" font-scale/body "font scale"))))

  (pretty-hydra-define font-scale (:color blue :title "Font Scale Panel")
    (""
     (("+" (default-text-scale-increase) "zoom in")
      ("-" (default-text-scale-decrease) "zoom out")
      ("0" (default-text-scale-reset) "reset"))))

  (pretty-hydra-define hydra-motions (:color blue :title "Motions")
    ("Jump"
     (("l" avy-goto-line "goto line")
      ("w" avy-goto-word-1 "goto word")
      ("c" avy-goto-char-2 "goto char"))
     "Expand"
     (("e" er/expand-region)      
      ("p" er/mark-inside-pairs)
      ("q" er/mark-inside-quotes))
     ))

  (pretty-hydra-define hydra-comments (:hint nil :color teal :exit t :title "Commentary Actions")
    (""
     (("b" comment-box)
      ("c" comment-dwim)
      ("l" comment-line)
      ("r" comment-region))))

  (pretty-hydra-define hydra-toggles
    (
     :pre (setq which-key-inhibit t)
     :post (setq which-key-inhibit nil)
     :title (with-faicon "toggle-on" "Toggles")
     :foreign-keys warn
     :quit-key "q"
     :exit t
     )
    ("Info/check/linting Modes"
     (("e" eldoc-mode "Echo Lisp objs" :toggle t)
      ("a" apheleia-mode "Code format" :toggle t)
      ("A" apheleia-global-mode "Format global" :toggle t)
      ("fc" flycheck-mode "Code linter" :toggle t)
      ("fs" flyspell-mode "Spell check" :toggle t)
      ("fp" flyspell-prog-mode "Spell check prog" :toggle t)
      ("fv" flycheck-verify-setup "Verify setup")
      ("ld" lsp-ui-doc-mode :toggle t)
      ("lp" lsp-ui-peek-mode :toggle t)
      ("ls" lsp-ui-sideline-mode :toggle t))
     "Edit/assistance"
     (("C-p" persp-mode-projectile-bridge-mode "Projectile bridge mode" :toggle t)
      ("C-j" ja-keys-minor-mode "My keys minor mode" :toggle t)
      ("C-A" global-auto-complete-mode "AC global" :toggle t)
      ("C-a" auto-complete-mode "AC local" :toggle t)
      ("C-l" electric-layout-mode "Elec layout" :toggle t)
      ("C-i" electric-indent-local-mode "Elec indent" :toggle t)
      ("C-q" electric-quote-local-mode "Elec quote" :toggle t)
      ("C-g" aggressive-indent-mode "Aggro indent" :toggle t)
      ("C-w" toggle-word-wrap "Word wrap" :toggle t)
      ("C-t" toggle-truncate-lines "Trunc lines" :toggle t)
      ("C-s" yas-minor-mode "Yas" :toggle t)
      ("C-c" whitespace-cleanup-mode "Whtspc cleanup" :toggle t)
      ("C-f" auto-fill-mode "Autofill" :toggle t) ; TODO: Toggle face does not change
      ("C-y" lispy-mode "Lispy" :toggle t))
     "Visual"
     (("e" jawa/toggle-org-emphasis-markers "Org emphasis" :toggle t)
      ("o" origami-mode "Origami" :toggle t)
      ("n" linum-mode "Linum" :toggle t)
      ("w" whitespace-mode "Whtspc" :toggle t)
      ("p" page-break-lines-mode "Page break lines" :toggle t)
      ("g" global-git-gutter-mode "Git gutter" :toggle t)
      ("i" fci-mode "Fill column ind" :toggle t)
      ("C-i" highlight-indent-guides-mode "Hilite indent" :toggle t)
      ("C-r" ivy-filthy-rich-mode "Ivy filty rich" :toggle t)
      ("ESC" nil "Quit"))))

  (pretty-hydra-define hydra-projects (:color blue :title "Projects")
    ("project actions"
     (("p" counsel-projectile "counsel-projectile")
      ("b" counsel-projectile-switch-to-buffer "project buffers")
      ("S" counsel-projectile-switch-project "switch project")
      ("s" counsel-projectile-rg "project search")
      ("f" counsel-projectile-find-file "find file in project" :exit t)
      ("d" counsel-projectile-find-dir "find dir in project" :exit t)
      ("i" projectile-invalidate-cache :color blue)
      )))

  (pretty-hydra-define hydra-buffers (:hint nil :color teal :title "Buffer Management Commands")
    ("Actions"
     (("b" counsel-switch-buffer "switch-buffer")
      ("d" kill-this-buffer)
      ("O" kill-other-buffers)
      ("m" switch-to-modified-buffer)
      ("s" swiper "search")
      ("i" counsel-imenu "fuzzy-search-imenu")
      ("S" switch-to-scratch))))

  (pretty-hydra-define hydra-files (:hint nil :color teal :title "Files Commands")
    ("Find"
     (("f" counsel-find-file "find-file" :exit t)
      ("e" open-init-el "open init.el" :exit t)
      ("r" counsel-recentf "find recentf" :exit t))))

  (pretty-hydra-define hydra-windows (:hint nil :title "Window Management")
    ("Switch"
     (("d" delete-window "delete window" :exit t)
      ("o" other-window "select other window" :exit t)
      ("O" delete-other-windows "delete other windiws" :exit t)
      ("w" ace-window "select window" :exit t)
      ("|" split-window-right "split window right" :exit t)
      ("-" split-window-below "split-window-below" :exit t))
     "Resize"
     (("+" enlarge-window "increase window")
      ("-" shrink-window "decrease window")
      ("max" maximize-window "maximize window")
      ("min" minimize-window "minimize window"))
     "Movement"
     (("h" windmove-left )
      ("j" windmove-down )
      ("k" windmove-up )
      ("l" windmove-right ))
     "Winner"
     (("u" winner-undo)
      ("U" winner-redo))
     ))

  (pretty-hydra-define hydra-lsp (:title "LSP Commands")
    ("Server"
     (("M-s" lsp-describe-session)
      ("M-r" lsp-restart-workspace)
      ("S" lsp-shutdown-workspace))
     "Buffer"
     (("f" lsp-format-buffer "format")
      ("m" lsp-ui-imenu "imenu")
      ("x" lsp-execute-code-action "execute action"))
     "Symbol"
     (("d" lsp-find-declaration "declaration")
      ("D" lsp-ui-peek-find-definitions "definition")
      ("R" lsp-ui-peek-find-references "references")
      ("l" lsp-ivy-workspace-symbol "symbol")
      ("L" lsp-ivy-global-workspace-symbol "symbol(global)"))
     ""
     (("i" lsp-ui-peek-find-implementation "implementation")
      ("t" lsp-find-type-definition "type")
      ("s" lsp-signature-help "signature")
      ("o" lsp-describe-thing-at-point "documentation")
      ("r" lsp-rename "rename"))
     ))
  )
;; Prefer function aliases
(defalias 'my-M-x           'counsel-M-x)
(defalias 'my-switch-buffer 'counsel-switch-buffer)
(defalias 'my-mini          'counsel-buffer-or-recentf)
(defalias 'my-find-file     'counsel-find-file)
(defalias 'my-find-recentf  'counsel-buffer-or-recentf)

;; Global
(global-set-key (kbd "<f1> f")	'counsel-describe-function)
(global-set-key (kbd "<f1> m")	'counsel-describe-map)
(global-set-key (kbd "<f1> s")	'counsel-describe-symbol)
(global-set-key (kbd "<f1> v")	'counsel-describe-variable)
;; (global-set-key (kbd "<f8>")	'my/treemacs-select-window)
(global-set-key (kbd "C-`")	'toggle-eshell-project-root)
(global-set-key (kbd "C-/")	'comment-line)
(global-set-key (kbd "C-M-l")	'indent-whole-buffer)
(global-set-key (kbd "C-j")	'ace-window)
(global-set-key (kbd "C-s")	'swiper-thing-at-point)
(global-set-key (kbd "C-x C-f")	'my-find-file)
(global-set-key (kbd "C-x b")	'my-mini)
(global-set-key (kbd "C-x c b")	'ivy-resume)
(global-set-key (kbd "M-x")	'my-M-x)
(global-set-key (kbd "≈")	'my-M-x)
(global-set-key (kbd "C-c q r") 'restart-emacs)
(global-set-key (kbd "C-=")	'er/expand-region)
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)
(global-set-key (kbd "C-c l")	'avy-goto-line)
(global-set-key (kbd "C-h m") 'describe-mode)

;; Mirror Mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") #'counsel-company))

;; Evil
(with-eval-after-load 'evil-maps
  ;; Normal state
  (define-key evil-normal-state-map "J"	 'evil-scroll-page-down)
  (define-key evil-normal-state-map "K"	 'evil-scroll-page-up)
  (define-key evil-normal-state-map "u"	 'undo-tree-undo)
  (define-key evil-normal-state-map "U"	 'undo-tree-redo)
  (define-key evil-normal-state-map "gj" 'evil-join)
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-launcher/body)
  ;; (define-key evil-normal-state-map (kbd "\\") 'hydra-launcher/body)

  ;; Visual state
  (define-key evil-visual-state-map (kbd "SPC") 'hydra-launcher/body)
  ;; (define-key evil-visual-state-map (kbd "\\") 'hydra-launcher/body)

  ;; Insert state
  (define-key evil-insert-state-map "\C-e" 'move-end-of-line)
  (define-key evil-insert-state-map "\C-a" 'move-begining-of-line))

;;; Golang:
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . go-mode-hook-func))
  :bind (:map go-mode-map
	      ("C-c d d" . godef-describe)
	      ("C-c d p" . godoc-at-point)
	      ("C-c r u" . go-remove-unused-imports)
	      ("C-M-l" . gofmt))
  :init
  ;; Copy system environment variables
  (when (memq window-system '(mac ns x))
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
      (unless (getenv var)
	(exec-path-from-shell-copy-env var))))

  (defun go-mode-hook-func ()
    (setq tab-width 4
	  indent-tabs-mode 1)
    (subword-mode 1)
    (lsp-deferred)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  )

(use-package flycheck-golangci-lint
  :if (executable-find "golangci-lint")
  :after flycheck
  :defines flycheck-disabled-checkers
  :hook (go-mode . (lambda ()
		     "Enable golangci-lint."
		     (setq flycheck-disabled-checkers '(go-gofmt
							go-golint
							go-vet
							go-build
							go-test
							go-errcheck))
		     (flycheck-golangci-lint-setup))))

(use-package go-tag
  :bind (:map go-mode-map
	      ("C-c t" . go-tag-add)
	      ("C-c T" . go-tag-remove))
  :config (setq go-tag-args (list "-transform" "camelcase")))

(use-package gotest
  :bind (:map go-mode-map
	      ("C-c a" . go-test-current-project)
	      ("C-c m" . go-test-current-file)
	      ("C-c ." . go-test-current-test)
	      ("C-c x" . go-run)))

(use-package go-gen-test
  :bind (:map go-mode-map
	      ("C-c C-t" . go-gen-test-dwim)))

;; use lsp instead
;; (use-package go-guru
;;   :bind (:map go-mode-map
;; 	      ([remap xref-find-definitions] . go-guru-definition)
;; 	      ([remap xref-find-references] . go-guru-referrers)))

(use-package go-projectile
  :after projectile
  :commands (go-projectile-mode go-projectile-switch-project)
  :hook ((go-mode . go-projectile-mode)
	 (projectile-after-switch-project . go-projectile-switch-project)))

(use-package go-add-tags)
(use-package go-dlv)
(use-package go-fill-struct)
(use-package go-impl)
(use-package go-playground)
(use-package go-rename)
(use-package go-snippets)
(use-package golint)
(use-package govet)

(use-package flycheck-gometalinter
  :init
  (setq flycheck-gometalinter-vendor t)
  :config
  (progn
    (flycheck-gometalinter-setup)))

;;; Eshell:
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-did-you-mean
  :config (eshell-did-you-mean-setup))

(use-package eshell-up)

(use-package esh-help
  :config (setup-esh-help-eldoc))

(use-package eshell-z)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :demand t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

;; 在project根目录下打开eshell
(setq toggle-eshell--last-buffer "*scratch*")

(defun toggle-eshell-project-root ()
  (interactive)
  (if (string-prefix-p "*eshell" (buffer-name)) (switch-to-buffer toggle-eshell--last-buffer)
    (progn
      (setq toggle-eshell--last-buffer (buffer-name))
      (message (format "switch to eshell from %s" (buffer-name)))
      (projectile-run-eshell nil))))

(global-set-key (kbd "<f7>") 'toggle-eshell-project-root)

;;; Python:
(use-package python-mode
  :init
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))

  :hook (python-mode .(lambda ()
			(eldoc-mode 0))))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

;;; Rust:
(use-package rust-mode
  :hook (rust-mode . rust-mode-hook-func)
  :init
  (setq rust-format-on-save t
	lsp-rust-server 'rust-analyzer)
  (defun rust-mode-hook-func ()
    (lsp-deferred)
    (when use-tabnine
      (add-to-list 'company-backends #'company-tabnine))))

(use-package rust-playground)

(defun init-rust-racer()
  (use-package racer
    :init(unless (getenv "RUST_SRC_PATH")
	   (setenv "RUST_SRC_PATH"
		   "/Users/guangshengli/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
		   )))

  (use-package company-racer
    :config
    (add-to-list 'company-backends 'company-racer)))

;;; C/C++:
;; Installation:
;;   brew install ccls
(use-package ccls
  :defer t
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp)))
  :init
  (setq ccls-initialization-options '(:index (:comments 2)
					     :completion (:detailedLabel t)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
	  (append '("compile_commands.json"
		    ".ccls")
		  projectile-project-root-files-top-down-recurring))))

;;; Ruby:
(use-package ruby-mode :defer t)

;; markdown-mode
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode-hook . (lambda ()
				(set-face-attribute 'markdown-table-face nil 
						    ;; :family "Noto Sans Mono CJK SC"
						    :family "Iosevka"
						    :weight 'normal
						    :width 'normal))))

;; org-mode
(use-package org-bullets
  :defer t
  :init
  (setq
   org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "ABORT(a)"))
   org-todo-keyword-faces '(("TODO" . "red")
			    ("DOING" . "yellow")
			    ("DONE" . "green")
			    ("ABORT" . "blue")))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode 1)
	      ;; 在org-table中使用中英文等宽的字体使表格框线对齐
	      (set-face-attribute 'org-table nil 
				  ;; :family "Noto Sans Mono CJK SC"
				  :family "Iosevka"
				  :weight 'normal
				  :width 'normal))))

(use-package lua-mode
  :defer t
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode)
  :hook (lua-mode . lsp-deferred))

(use-package typescript-mode
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)
	 ("\\.tsx\\'" . typescript-mode)))

(use-package json-mode
  :defer t
  :mode (("\\.json\\'" . json-mode))
  :hook (json-mode . lsp-deferred))

(use-package toml-mode
  :defer t
  :mode (("\\.toml\\'" . toml-mode)))

(use-package yaml-mode
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(use-package powershell
  :defer t
  :hook (powershell-mode . lsp-deferred))

(use-package bazel
  :defer t
  :mode (("WORKSPACE\\'" . bazel-mode)
	 ("BUILD\\'"     . bazel-mode)))

(use-package graphql-mode  :defer t)

(use-package protobuf-mode :defer t)

(use-package groovy-mode :defer t)

(use-package dockerfile-mode :defer t)

;;; Posframe:
(use-package posframe
  :if prefer-posframe)

(use-package ivy-posframe
  :if prefer-posframe
  :init (setq ivy-posframe-border-width 2
	      ivy-posframe-display-functions-alist '((complete-symbol . ivy-posframe-display-at-point)
						     (t . ivy-posframe-display)))
  :config (ivy-posframe-mode 1))

(use-package which-key-posframe
  :if prefer-posframe
  :init (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center
	      which-key-posframe-border-width 2
	      which-key-posframe-parameters '((left-fringe . 5) (right-fringe . 5)))
  :config (which-key-posframe-mode))

;; (use-package hydra-posframe
;;   :if prefer-posframe
;;   :load-path "~/.emacs.d/site-lisp/hydra-posframe"
;;   :hook (after-init . hydra-posframe-mode)
;;   :init
;;   (setq hydra-posframe-border-width 2
;; 	hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center
;; 	hydra-posframe-parameters '((left-fringe . 5)(right-fringe . 5)))
;;   :custom-face (hydra-posframe-border-face ((t (:background "#bf616a"))))
;;   :custom-face (hydra-posframe-face ((t (:background "#3b4252"))))
;;   )


;;; Theme:
(use-package doom-themes
  :hook (emacs-startup . (lambda () (load-theme 'doom-vibrant t)))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; Modeline:
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config  (setq doom-modeline-height 25
		 doom-modeline-bar t
		 doom-modeline-bar-width 10
		 doom-modeline-hud nil
		 doom-modeline-window-width-limit fill-column
		 doom-modeline-project-detection 'project
		 doom-modeline-buffer-file-name-style 'relative-to-project
		 doom-modeline-icon (display-graphic-p)
		 doom-modeline-modal-icon nil
		 doom-modeline-major-mode-icon t
		 doom-modeline-buffer-state-icon t
		 doom-modeline-buffer-modification-icon t
		 doom-modeline-number-limit 99
		 doom-modeline-lsp t
		 doom-modeline-enable-word-count t
		 doom-modeline-buffer-encoding t
		 doom-modeline-indent-info t
		 doom-modeline-workspace-name t
		 doom-modeline-env-go-executable "go"))

;;; Fonts:
;; recommend: https://github.com/powerline/fonts
;; Font Example:
;; 春眠不觉晓，处处闻啼鸟
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; 0123456789

(defvar prefer-cn-font nil)

(setq-default cjk-charsets  '(kana han symbol cjk-misc bopomofo)
	      default-unicode-fonts '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
	      default-cn-fonts      `(,(font-spec :family "WenQuanYi Micro Hei" :height 90)
				      ,(font-spec :family "Microsoft Yahei" :height 90))
	      default-en-fonts      '("Droid Sans Mono:size=14"
				      "Menlo:size=14"
				      "Monoco:size=14"
				      "Consolas:size=14"
				      "Courier New:size=14"
				      "monospace:size=14"))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  (interactive)
  ;; set unicode fonts
  (cl-loop for font in default-unicode-fonts
	   when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; set en fonts
  (let ((fonts (copy-sequence default-en-fonts)))
    (when (or (stringp prefer-en-font) (fontp prefer-en-font)) (push prefer-en-font fonts))
    (cl-loop for font in fonts
	     when (font-installed-p font)
	     return (set-frame-font font)))

  ;; set cjk fonts
  (let ((fonts (copy-sequence default-cn-fonts)))
    (when (or (stringp prefer-cn-font) (fontp prefer-cn-font)) (push prefer-cn-font fonts))
    (cl-loop for font in fonts
	     do (cl-loop for charset in cjk-charsets
			 do (set-fontset-font t charset font))))
  )

(when (display-graphic-p)
  (setq prefer-en-font "Go Mono for Powerline:size=14")
  (add-hook 'after-init-hook 'setup-fonts)
  (add-hook 'minibuffer-setup-hook '(lambda () (set (make-local-variable 'face-remapping-alist) '((default :height 90))))))

;;; Server:
(use-package server
  :commands (server-running-p server-start)
  :hook (after-init . (lambda () (unless (server-running-p) (server-start)))))

;;; Experimental:
(use-package esup)

;;; Load custom file:
(when (and custom-file (file-readable-p custom-file) (load custom-file)))

;;; init.el ends here

