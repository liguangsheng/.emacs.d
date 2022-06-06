(use-package s)
(use-package dash)
(use-package shut-up)
(use-package restart-emacs)

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

;; 跳转
(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  ("C-c C-j" . avy-resume)

  :init
  (avy-setup-default)
  (my-leader-def
    "'" 'avy-goto-char-2)
  )

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

;; 高亮对应的paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
	      show-paren-when-point-in-periphery t))

;; 自动保存scratch buffer
(use-package persistent-scratch
  :disabled
  :init
  (setq persistent-scratch-autosave-interval 60)
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

;; (use-package exec-path-from-shell
;;   :config
;;   (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-initialize)))

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
  :hook (prog-mode . format-all-mode)
  :bind ([remap format-buffer] . format-all-buffer))

(use-package zoom-window
  :init
  (my-leader-def
    "w m" 'zoom-window-zoom
    "w n" 'zoom-window-next))

(provide 'init-features)
