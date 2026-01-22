(use-package s)
(use-package dash)
(use-package shut-up)
(use-package restart-emacs)
(use-package posframe)

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil
        undo-tree-enable-undo-in-region nil))

(use-package winner
  :config (winner-mode 1))

(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease default-text-scale-reset)
  :custom ((default-text-scale-amount 5)))

;; 平滑滚动屏幕
(when (and *gui* *emacs28+*)
  (use-package good-scroll
    :config
    (good-scroll-mode 1)))

;; 扩展选择区域
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; 彩虹分隔符
(use-package rainbow-delimiters
  :hook ((text-mode prog-mode fundamental-mode) . #'rainbow-delimiters-mode))

;; 高亮缩进
(use-package highlight-indentation
  :disabled
  :hook ((text-mode prog-mode fundamental-mode) . #'highlight-indentation-current-column-mode))

;; 高亮数字
(use-package highlight-numbers
  :hook ((text-mode prog-mode fundamental-mode) . #'highlight-numbers-mode))

;; 高亮TODO
(use-package hl-todo
  :init (global-hl-todo-mode))

(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; 自动保存scratch buffer
(use-package persistent-scratch
  :disabled
  :init
  (setq persistent-scratch-autosave-interval 60)
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package vi-tilde-fringe
  :init
  (global-vi-tilde-fringe-mode))

(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all)
		 :map symbol-overlay-map
		 ([M-up] . symbol-overlay-jump-prev)
		 ([M-down] . symbol-overlay-jump-next)
		 )
  )

;; 将buffer中#000000样式的16进制rgb渲染出染色
;; 需要的时候手动开启M-x rainbow-mode
(use-package rainbow-mode
  :defer t
  :commands (rainbow-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; (withf 'shut-up (shut-up (yas-global-mode 1))))
  (yas-global-mode 1))

(use-package format-all
  :bind ("C-c C-f" . format-all-buffer)
  :commands (define-format-all-formatter)
  :init
  (setq format-all-formatters '(("Go" goimports)
								("Shell" shfmt)
								("GraphQL" prettier)
								("Protocol Buffer" my-format-buffer)))
  )

(when my-auto-restore-session
  (use-package desktop
	:init
	;; 在 after-init-hook 中延迟执行 desktop-read 函数，自动恢复会话
	(add-hook 'after-init-hook 'desktop-read)

	:config
	;; 设置 desktop 文件保存的目录和文件名
	(setq desktop-dirname             (expand-user-var "desktop")
          desktop-base-file-name      "emacs.desktop"
          desktop-base-lock-name      "emacs.desktop.lock"
          desktop-path                (list desktop-dirname)
          desktop-save                t
          desktop-files-not-to-save   "^$" ; 过滤掉不保存的文件（例如reload tramp paths）
          desktop-load-locked-desktop nil
          desktop-auto-save-timeout   30)

	(add-hook 'kill-emacs-hook 'desktop-save-in-desktop-dir) ; 在退出 Emacs 时保存会话状态

	:hook
	(emacs-startup . desktop-save-mode) ; 在 Emacs 启动时启用 desktop-save-mode
	))

(use-package ansi-color
  :config
  (defun display-ansi-color ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

  (add-hook 'find-file-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match-p "\\.log\\'" buffer-file-name))
                (display-ansi-color)))))

(provide 'init-features)
