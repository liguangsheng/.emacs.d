(use-package  rust-mode
  :init (setq rust-format-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-mode))

(use-package rust-playground)

(provide 'init-rust)
