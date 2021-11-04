(use-package counsel
  :init (setq ivy-height 30
			  ivy-initial-inputs-alist nil)
  :config
  (defun counsel-rg-dir ()
    "在指定文件夹下进行搜索，先到dired选择文件夹，运行此函数"
    (interactive)
    (counsel-rg "" (dired-file-name-at-point)))
  )

(use-package smex)

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package ivy-hydra  :after (:all ivy hydra))

(provide 'init-ivy)
