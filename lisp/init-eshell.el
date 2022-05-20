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

(provide 'init-eshell)
