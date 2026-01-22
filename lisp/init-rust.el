;;; init-rust.el --- Rust language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Rust development with rustic mode and LSP support.

;;; Code:

(use-package rustic
  :custom
  (rustic-lsp-server 'rust-analyzer))

(use-package rust-playground)

(provide 'init-rust)

;;; init-rust.el ends here
