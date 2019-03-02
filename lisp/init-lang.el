;;; init-lang.el -- language configuration

;;; Commentary:

;;; Code:

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
    ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
    ;; (add-hook 'before-save-hook #'lsp-organize-imports t t))
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

(provide 'init-lang)
;;; init-lang.el ends here
