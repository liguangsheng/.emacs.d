(use-package rust-mode
  :hook (rust-mode . rust-mode-hook-func)
  :init
  (setq rust-format-on-save t
	lsp-rust-server 'rust-analyzer)
  (defun rust-mode-hook-func ()
    (lsp-deferred)
    ;; (when preferences/enable-tabnine
    ;;   (add-to-list 'company-backends #'company-tabnine)))
  ))

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

(provide 'init-rust)
