;; (use-package robe
;;   :diminish robe-mode
;;   :defines company-backends
;;   :hook (ruby-mode . robe-mode)
;;   :config
;;   (with-eval-after-load 'company
;;     (cl-pushnew 'company-robe company-backends)))

(use-package ruby-mode
  :ensure nil
  :mode "\\.\\(rb\\|rake\\|\\gemspec\\|ru\\|\\(Rake\\|Gem\\|Guard\\|Cap\\|Vagrant\\)file\\)$"
  :interpreter "ruby"
  :config
  ;; Code navigation, documentation lookup and completion for Ruby
  (add-hook 'ruby-mode-hook #'lsp)

  ;; Ruby refactoring helpers
  (use-package ruby-refactor
    :diminish ruby-refactor-mode
    :hook (ruby-mode . ruby-refactor-mode-launch))

  ;; Run a Ruby process in a buffer
  (use-package inf-ruby
    :hook ((ruby-mode . inf-ruby-minor-mode)
	   (compilation-filter . inf-ruby-auto-enter)))

  ;; Rubocop
  ;; Install: gem install rubocop
  (use-package rubocop
    :diminish rubocop-mode
    :hook (ruby-mode . rubocop-mode))

  ;; RSpec
  (use-package rspec-mode
    :diminish rspec-mode
    :commands rspec-install-snippets
    :hook (dired-mode . rspec-dired-mode)
    :config (with-eval-after-load 'yasnippet
	      (rspec-install-snippets)))

  ;; Coverage for SimpleCov
  (use-package coverage)

  ;; Yet Another RI interface for Emacs
  (use-package yari
    :bind (:map ruby-mode-map ([f1] . yari)))

  ;; Ruby YARD comments
  (use-package yard-mode
    :diminish yard-mode
    :hook (ruby-mode . yard-mode)))

;; YAML mode
(use-package yaml-mode)

(provide 'init-ruby)
