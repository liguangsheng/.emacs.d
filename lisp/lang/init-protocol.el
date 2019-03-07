(use-package protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

(provide 'init-protocol)
