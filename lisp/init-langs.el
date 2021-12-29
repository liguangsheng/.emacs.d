;;; init-lang.el -- language configuration

;;; Commentary:

;;; Code:

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

;;; Haskell:
(use-package haskell-mode)

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
  :mode (("\\.json\\'" . json-mode)))

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

(use-package graphql-mode)

(use-package protobuf-mode)

(use-package groovy-mode)

(use-package dockerfile-mode)

(provide 'init-langs)
;;; init-lang.el ends here
