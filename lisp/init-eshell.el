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

(provide 'init-eshell)
