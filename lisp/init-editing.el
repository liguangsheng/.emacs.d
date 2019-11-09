;; 智能括号
(defvar smartparens-p nil)
(use-package smartparens
  :config
  (sp-with-modes
      '(c++-mode objc-mode c-mode go-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (when smartparens-p (smartparens-global-mode)))

;; 平滑滚动屏幕
(defvar smooth-scrolling-p nil)
(use-package smooth-scrolling
  :init
  (setq
   smooth-scroll-margin 1
   smooth-scroll-strict-margins t)
  :config
  (when smooth-scrolling-p (smooth-scrolling-mode 1)))

;; 扩展选择区域
(use-package expand-region
  :config
  (evil-leader/set-key
    "ep" 'er/mark-inside-pairs
    "eq" 'er/mark-inside-quotes
    "eu" 'er/mark-url
    "ee" 'er/mark-email
    "ea" 'er/mark-text-paragraph
    "ev" 'er/expand-region
    "v" 'er/expand-region
    ))

;; 跳转
(use-package avy
  :init
  (evil-leader/set-key
    "SPC" 'avy-goto-word-1
    "l" 'avy-goto-line
    ))

;; 切换到修改过但没保存的的buffer
(defun switch-to-modified-buffer ()
  (interactive)
  (let ((buf-list (seq-filter (lambda (x)
				(not
				 (or
				  (not (buffer-modified-p x))
				  (s-prefix? "*" (buffer-name x))
				  (s-prefix? " *" (buffer-name x))
				  (s-suffix? "-mode" (buffer-name x)))))
			      (buffer-list))))
    (if buf-list
	(switch-to-buffer (first buf-list))
      (message "No buffer modified."))))

(evil-leader/set-key
  "bm" 'switch-to-modified-buffer)

(provide 'init-editing)
