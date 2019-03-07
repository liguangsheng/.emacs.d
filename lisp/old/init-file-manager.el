(use-package find-file-in-project)

(use-package ranger
  :init (evil-leader/set-key "fm" 'ranger)
  :config (ranger-override-dired-mode t))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


;; (use-package neotree
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
;;         neo-smart-open t)
;;   (define-key neotree-mode-map "j" 'neotree-next-line)
;;   (define-key neotree-mode-map "k" 'neotree-previous-line)
;;   (define-key neotree-mode-map "h" 'neotree-up-dir)
;;   (define-key neotree-mode-map "l" 'neotree-change-root)
;;   (define-key neotree-mode-map (kbd "RET") 'neotree-enter)

;;   (add-hook 'neotree-mode-hook (lambda () (linum-mode -1)) t)

;;   (add-hook 'neotree-mode-hook
;;             (lambda ()
;;               (define-key evil-normal-state-local-map "q" 'neotree-hide)
;;               (define-key evil-normal-state-local-map "j" 'neotree-next-line)
;;               (define-key evil-normal-state-local-map "k" 'neotree-previous-line)
;;               (define-key evil-normal-state-local-map "h" 'neotree-up-dir)
;;               (define-key evil-normal-state-local-map "l" 'neotree-change-root)
;;               (define-key evil-normal-state-local-map "A" 'neotree-stretch-toggle)
;;               (define-key evil-normal-state-local-map "H" 'neotree-hidden-file-toggle)
;;               (define-key evil-normal-state-local-map "f" 'neotree-find)
;;               (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
;;               (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
;;               (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look))))

;; (defun my/neotree-toggle ()
;;   (interactive)
;;   (let ((project-dir (ffip-project-root))
;;         (file-name (buffer-file-name)))
;;     (if project-dir
;;         (progn
;;           (neotree-dir project-dir)
;;           (neotree-find file-name))
;;       (progn
;;         (neotree-show)
;;         (message "Cound not find git project root.")))))

;; (defun neotree-up-dir ()
;;   (interactive)
;;   (beginning-of-buffer)
;;   (neotree-change-root))

(provide 'init-file-manager)
