;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(setq-default
 ;; 显示行号
 show-line-number-p t
 ;; 启动时窗口最大化
 maximize-frame-at-start-p t
 ;; 平滑滚动
 smooth-scrolling-p t
 ;; 自动重新加载被修改过的文件
 auto-revert-p t
 ;; 中英文字体
 ;; https://github.com/powerline/fonts
 ;; curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
 en-fonts '("Fira Mono for Powerline" 13 "Source Code Pro" 13 "Courier New" 13)
 cn-fonts '("华文细黑" 16 "宋体" 15 "微软雅黑" 15)
 ;; 使用主题
 theme 'doom-one-light
 ;; 是否启动emacs server
 server-p t
 server-socket-dir "/tmp/emacs-server/"
 server-name "emacs-server"
 )

;;; ----------------------------------------------------------------------------
;;; Core
(require 'cl-lib)

(defvar emacs-root-dir (file-truename user-emacs-directory)
  "Path to .emacs.d directory.")

(defvar emacs-lisp-dir  (expand-file-name "lisp/" emacs-root-dir)
  "Path to .emacs.d/lisp directory where init files exists.")

(defvar emacs-site-lisp-dir (expand-file-name "site-lisp/" emacs-root-dir)
  "Path to .emacs.d/site-lisp directory.")

;; Add lisp and core dir to load-path
(add-to-list 'load-path emacs-lisp-dir)

;; Add site-lisp to load-path
(let ((default-directory emacs-site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(defalias 'yes-or-no-p 'y-or-n-p) ;; use y/n insteal of yes/no
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(ignore-errors (savehist-mode 1))
(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; Show line number
(when show-line-number-p
  (if (fboundp 'display-line-numbers-mode)
      (global-display-line-numbers-mode 1)
    (global-linum-mode 1)))

;; Maximize frame at start
(defvar maximize-frame-at-start-p t "Maximize-frame-at-start-p.")
(when maximize-frame-at-start-p (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;; Use utf-8 as default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))               ; pretty
(prefer-coding-system                   'utf-8)  ; pretty
(set-terminal-coding-system             'utf-8)  ; pretty
(set-keyboard-coding-system             'utf-8)  ; pretty
(set-selection-coding-system            'utf-8)  ; pretty
(setq locale-coding-system              'utf-8)  ; pretty
(setq-default buffer-file-coding-system 'utf-8)  ; with sugar on top

;; Better variables
(setq
 apropos                      t
 backup-by-copying            t
 comint-prompt-read-only      t
 compilation-always-kill      t
 compilation-ask-about-save   nil
 compilation-scroll-output    t
 debug-on-error               t
 delete-old-versions          t
 gc-cons-threshold            2147483648 ; 2GB
 history-length               1024
 idle-update-delay            0.5
 inhibit-startup-message      t
 kept-new-versions            6
 kept-old-versions            2
 large-file-warning-threshold 100000000
 vc-follow-symlinks           t
 version-control              t
 visible-bell                 0
 )

;;; ----------------------------------------------------------------------------
;;; Package Manage(straight)

(require 'package)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; ----------------------------------------------------------------------------
;;; Init Font
(defvar en-fonts '("Source Code Pro" 13 "Courier New" 13))
(defvar cn-fonts '("宋体" 15 "微软雅黑" 15))

(defun init/exists-p (font-name)
  "检查字体是否存在."
  (if (null (x-list-fonts font-name)) nil t))

(defun init/use-en (font-name font-size)
  "设置英文字体."
  (set-face-attribute 'default nil
		      :font (format "%s:pixelsize=%d" font-name font-size)
		      :weight 'normal))

(defun init/use-cn (font-name font-size)
  "设置中文字体."
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
		      (font-spec :family font-name :size font-size))))

(defun init/use-list (font-list font-func)
  "设置字体列表，从列表头开始查找存在的字体并使用该字体."
  (unless (null font-list)
    (let ((font-name (car font-list))
	  (font-size (cadr font-list)))
      (if (init/exists-p font-name)
	  (funcall font-func font-name font-size)
	(init/use-list (cddr font-list) font-func)))))

(defun init/use-en-list (font-list)
  (init/use-list font-list 'init/use-en))

(defun init/use-cn-list (font-list)
  (init/use-list font-list 'init/use-cn))


(defun init-font ()
  (when (display-graphic-p)
    (init/use-en-list en-fonts)
    (init/use-cn-list cn-fonts)))

(init-font)

;;; ----------------------------------------------------------------------------
;;; Init Packages
(use-package evil
  :config
  (evil-ex-define-cmd "q" 'kill-this-buffer) ;; make :q just kill buffer, do not exit emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
  (evil-mode 1))

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  )

(use-package restart-emacs
  :config
  (evil-leader/set-key "qr" 'restart-emacs))


;; 智能括号
(defvar smartparens-p nil)
(use-package smartparens
  :config
  (sp-with-modes
      '(c++-mode objc-mode c-mode go-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (when smartparens-p (smartparens-global-mode)))

;; 平滑滚动屏幕
(defvar smooth-scrolling-p nil)
(use-package smooth-scrolling
  :init
  (setq
   smooth-scroll-margin 1
   smooth-scroll-strict-margins t)
  :config
  (when smooth-scrolling-p (smooth-scrolling-mode 1)))

;; 扩展选择区域
(use-package expand-region
  :config
  (evil-leader/set-key
    "ep" 'er/mark-inside-pairs
    "eq" 'er/mark-inside-quotes
    "eu" 'er/mark-url
    "ee" 'er/mark-email
    "ea" 'er/mark-text-paragraph
    "ev" 'er/expand-region
    "v" 'er/expand-region
    ))

;; 跳转
(use-package avy
  :init
  (evil-leader/set-key
    "SPC" 'avy-goto-word-1
    "l" 'avy-goto-line
    ))

;; helm
(use-package helm
  :bind ("M-x" . 'helm-M-x)
  :config
  (customize-set-variable 'helm-ff-lynx-style-map t)
  (customize-set-variable 'helm-imenu-lynx-style-map t)
  (customize-set-variable 'helm-semantic-lynx-style-map t)
  (customize-set-variable 'helm-occur-use-ioccur-style-keys t)
  (customize-set-variable 'helm-grep-use-ioccur-style-keys t)
  (evil-leader/set-key
    "bb" 'helm-mini
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "hi" 'helm-imenu
    "hr" 'helm-recentf
    "hk" 'helm-show-kill-ring
    ))

;; default variables
(defvar gui-theme 'wombat)
(defvar cli-theme 'wombat)

(use-package helm-themes)
(use-package zenburn-theme :defer t)
(use-package dracula-theme :defer t)
(use-package doom-themes :defer t)

(defvar theme nil)
(when theme
  (message (format "load theme %s" (symbol-name theme)))
  (load-theme theme t)
  )

;; 彩虹分隔符
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; 高亮缩进
(use-package highlight-indentation
  :disabled
  :config (add-hook 'prog-mode-hook #'highlight-indentation-current-column-mode))

;; 高亮数字
(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; 高亮TODO
(use-package hl-todo
  :config (global-hl-todo-mode))

;; 高亮symbol
(use-package highlight-symbol
  :config (highlight-symbol-mode))

;; 高亮当前行
(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t)))) ; FIXME: compatible with 27
  :hook (after-init . global-hl-line-mode))

;; 高亮对应的paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
	      show-paren-when-point-in-periphery t)
  :config
  (with-no-warnings
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
		  (goto-char pos)
		  (make-overlay (line-beginning-position)
				(line-end-position)))))
	(overlay-put ol 'display str)
	(overlay-put ol 'face
		     (or face '(:inherit highlight)))
	ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
	(delete-overlay show-paren--off-screen-overlay))
      ;; check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
		 (not (or cursor-in-echo-area
			  executing-kbd-macro
			  noninteractive
			  (minibufferp)
			  this-command))
		 (and (not (bobp))
		      (memq (char-syntax (char-before)) '(?\) ?\$)))
		 (= 1 (logand 1 (- (point)
				   (save-excursion
				     (forward-char -1)
				     (skip-syntax-backward "/\\")
				     (point))))))
	;; rebind `minibuffer-message' called by
	;; `blink-matching-open' to handle the overlay display
	(cl-letf (((symbol-function #'minibuffer-message)
		   (lambda (msg &rest args)
		     (let ((msg (apply #'format-message msg args)))
		       (setq show-paren--off-screen-overlay
			     (display-line-overlay
			      (window-start) msg ))))))
	  (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-modeline
  :init (setq doom-modeline-height 25
	      doom-modeline-bar 3
	      doom-modeline-buffer-file-name-style 'relative-to-project
	      doom-modeline-icon t
	      doom-modeline-major-mode-icon t
	      )
  :config (doom-modeline-mode 1))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package company
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 24            ; bigger popup window
	company-idle-delay .2               ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

(use-package helm-company)

(use-package projectile)

(use-package helm-projectile
  :config
  (evil-leader/set-key
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file-dwim
    "pg" 'helm-projectile-grep
    "pp" 'helm-projectile
    "pr" 'helm-projectile-recentf
    "ps" 'projectile-run-eshell
    ))

(use-package flycheck)

;; treemacs
(use-package treemacs
  :config
  (treemacs-resize-icons 12)
  (defun treemacs-switch-window ()
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
	(aw-flip-window)
      (progn
	(aw--push-window (selected-window))
	(treemacs-select-window))))

  (evil-leader/set-key
    "-"  'treemacs-switch-window
    "="  'helm-treemacs-workspace
    "tr" 'treemacs
    "tt" 'treemacs-switch-window
    "tw" 'treemacs-switch-workspace
    "tp" 'treemacs-add-and-display-current-project
    ))

(defun move-to-front (list x)
  (cons x (remove x list)))

(setq helm--treemacs-last-candidate "Default")

(defun helm--treemacs-workspace-candidates ()
  (move-to-front
   (cl-loop for ws in (treemacs-workspaces) collect (treemacs-workspace->name ws))
   helm--treemacs-last-candidate))


(defun treemacs-find-workspace (name)
  (seq-find
   (lambda (x) (string-equal name (treemacs-workspace->name x)))
   (treemacs-workspaces)))

(defun treemacs-select-workspace (ws)
  (setf (treemacs-current-workspace) ws)
  (treemacs--invalidate-buffer-project-cache)
  (treemacs--rerender-after-workspace-change)
  (run-hooks 'treemacs-switch-workspace-hook))

(defun treemacs-select-workspace-by-name (name)
  (treemacs-select-workspace (treemacs-find-workspace name))
  (message "treemacs select workspace: %s" name))

(defun helm-treemacs-workspace ()
  (interactive)
  (helm :sources (helm-build-sync-source "Helm-Treemacs"
		   :candidates (helm--treemacs-workspace-candidates)
		   :fuzzy-match t
		   :action (lambda (candidate)
			     (setq helm--treemacs-last-candidate (treemacs-workspace->name (treemacs-current-workspace)))
			     (treemacs-select-workspace-by-name candidate))
		   )
	:buffer "*helm treemacs*"))

(use-package treemacs-projectile)

(use-package treemacs-evil)

;; server
(defvar server-p nil
  "Do you want start a emacs server")

(defun emacs-server-exist-p ()
  (file-exist-p (server-socket-path)))

(defun restart-emacs-server ()
  (interactive)
  (server-force-delete)
  (server-start))

(when server-p
  (restart-emacs-server))

;; lsp
(use-package lsp-mode
  :diminish lsp-mode
  :hook (prog-mode . lsp)
  :bind (("s-b" . xref-find-definitions)
	 ("s-]" . xref-find-definitions)
	 ("s-[" . evil-jump-backward))
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-diagnostic-functions '(lsp--flymake-backend nil))
  :config
  (setq lsp-inhibit-message t
	lsp-message-project-root-warning t
	create-lockfiles nil
	)

  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  (require 'lsp-clients)

  ;; C/C++ Mode
  ;; brew install ccls
  (use-package ccls
    :defines projectile-project-root-files-top-down-recurring
    :hook ((c-mode c++-mode objc-mode cuda-mode) .
	   (lambda () (require 'ccls) (lsp)))
    :init
    (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
    :config
    (with-eval-after-load 'projectile
      (setq projectile-project-root-files-top-down-recurring
	    (append '("compile_commands.json"
		      ".ccls")
		    projectile-project-root-files-top-down-recurring))))

  ;; python-mode
  (add-hook 'python-mode 'lsp)
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  )


(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq scroll-margin 0))

;; org-mode
(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ox-reveal)
(use-package org-re-reveal)

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)))

;; toml
(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

;; yaml
(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)))

;; json
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

;; go
(use-package go-mode
  :init
  ;; 环境变量
  (when (memq window-system '(mac ns x))
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
      (unless (getenv var)
	(exec-path-from-shell-copy-env var))))

  :config
  ;; 寻找goretuens作为格式化工具
  ;; go get -u -v github.com/sqs/goreturns
  (when (executable-find "goreturns")
    (setq gofmt-command "goreturns"))

  (add-hook 'go-mode-hook
	    (lambda ()
	      ;; 保存buffer之前格式化文件
	      (add-hook 'before-save-hook 'gofmt-before-save t)

	      ;; eyes and hands comfort
	      (subword-mode 1)
	      (setq tab-width 4)
	      (setq indent-tabs-mode 1)

	      (evil-leader/set-key
		"mdd" 'godef-describe)

	      (bind-key "s-]" 'godef-jump go-mode-map)
	      (bind-key "s-[" 'pop-tag-mark go-mode-map)

	      ;; Go support for lsp-mode using Sourcegraph's Go Language Server
	      ;; go get -u -v github.com/sourcegraph/go-langserver
	      (require 'lsp-go)
	      ))

  :bind (:map go-mode-map
	      ("s-[" . xref-find-definitions)
	      ("s-]" . xref-find-references)
	      )
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

;; (use-package go-eldoc
;;   :hook (go-mode . go-eldoc-setup))

(use-package go-guru
  :bind (:map go-mode-map
	      ([remap xref-find-definitions] . go-guru-definition)
	      ([remap xref-find-references] . go-guru-referrers)))

(with-eval-after-load 'company
  (use-package company-go
    :defines company-backends
    :init (cl-pushnew 'company-go company-backends)))

(with-eval-after-load 'projectile
  (use-package go-projectile
    :commands (go-projectile-mode go-projectile-switch-project)
    :hook ((go-mode . go-projectile-mode)
	   (projectile-after-switch-project . go-projectile-switch-project))))

(use-package go-add-tags)
(use-package go-dlv)
(use-package go-fill-struct)
(use-package go-impl)
(use-package go-playground)
(use-package go-rename)
(use-package go-snippets)
(use-package golint)
(use-package govet)

;; rust
(use-package  rust-mode
  :init (setq rust-format-on-save t))

(use-package rust-playground)

;; lua
(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode))

;; typescript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)))

;; bazel
(use-package bazel-mode
  :mode (("WORKSPACE\\'" . bazel-mode)
	 ("BUILD\\'" . bazel-mode)))

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

;;; ----------------------------------------------------------------------------
;;; My Functions

(defun kill-all-buffers (KILL-STARRED-BUFFER)
  "Kill all buffers."
  (dolist (buffer (buffer-list))
    (let ((bname (string-trim (buffer-name buffer))))
      (if (and (not KILL-STARRED-BUFFER)
	       (string-prefix-p "*" bname)
	       (string-suffix-p "*" bname))
	  nil
	(kill-buffer-if-not-modified buffer)
	))))

(defun kill-all-buffers-i ()
  (interactive)
  (kill-buffers nil))

(defun switch-to-modified-buffer ()
  "Switch to modified buffer"
  (interactive)
  (let ((buf-list (seq-filter (lambda (x)
				(not
				 (or
				  (not (buffer-modified-p x))
				  (s-prefix? "*" (buffer-name x))
				  (s-prefix? " *" (buffer-name x))
				  (s-suffix? "-mode" (buffer-name x)))))
			      (buffer-list))))
    (if buf-list
	(switch-to-buffer (first buf-list))
      (message "No buffer modified."))))

(defun open-init-el ()
  "Open ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun switch-to-scratch ()
  "Swtich to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; ----------------------------------------------------------------------------
;;; Init Keys

(which-key-add-key-based-replacements
  "SPC b" "buffer"
  "SPC c" "comment"
  "SPC e" "expand"
  "SPC f" "file"
  "SPC h" "helm"
  "SPC m" "mode"
  "SPC p" "projectile"
  "SPC q" "quit"
  "SPC t" "treemacs"
  "SPC w" "window"
  )

(bind-keys
 ("C-j" . ace-window))

(bind-keys
 :map evil-normal-state-map
 ("J" . evil-scroll-page-down)
 ("K" . evil-scroll-page-up)
 ("U" . undo-tree-redo)
 ("gJ" . evil-join)
 )

(bind-keys
 :map evil-motion-state-map
 ("K" . evil-scroll-page-up))

(evil-leader/set-key
  ;; file
  "fw" 'save-buffer
  "fe" 'open-init-el
  "fs" 'save-buffer

  ;; buffer
  "bd"  'kill-this-buffer
  "bD"  'kill-all-buffers-i
  "bs"  'switch-to-scratch
  "bm"  'switch-to-modified-buffer

  ;; window
  "wd"    'delete-window
  "wn"    'other-window
  "wo"    'delete-other-windows
  "w-"    'split-window-below
  "w|"    'split-window-right
  "ww"    'ace-window
  "w SPC" 'ace-window

  ;; comment
  "cl" 'comment-line
  "cc" 'comment-dwim

  ;; move
  "SPC"   'avy-goto-word-1
  "l"     'avy-goto-line

  ;; quit
  "qq" 'save-buffers-kill-emacs
  )

;;; init.el ends here
