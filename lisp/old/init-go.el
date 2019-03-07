;;; init-golang.el --- Configuration for golang

(defvar GOROOT (string-trim-right (shell-command-to-string "go env GOROOT")))
(defvar GOPATH (string-trim-right (shell-command-to-string "go env GOPATH")))
(defvar GOBIN  (string-trim-right (shell-command-to-string "go env GOBIN")))
(add-to-list 'exec-path GOBIN)

;; Go packages:
;; go get -u github.com/mdempsky/gocode # github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/gotype
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct

(defun go-mode-hook-func ()
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save t)

  ;; eyes and hands comfort
  (subword-mode 1)
  (setq tab-width 4)
  (setq indent-tabs-mode 1)

  (evil-leader/set-key
    "mdd" 'godef-describe)

  (bind-key "s-]" 'godef-jump go-mode-map)
  (bind-key "s-[" 'pop-tag-mark go-mode-map)
  )

(use-package go-mode
  :config 
  (setq gofmt-command "goreturns")
  (add-hook 'go-mode-hook 'go-mode-hook-func)
  )

(use-package go-add-tags)
(use-package go-dlv)
(use-package go-fill-struct)
(use-package go-impl)
(use-package go-rename)
(use-package go-snippets)
(use-package golint)
(use-package govet)

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
	      ;; ([remap xref-find-definitions] . go-guru-definition)
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

(provide 'init-go)
;;; init-go.el ends here

