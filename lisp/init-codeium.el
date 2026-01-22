;;; init-codeium.el --- Codeium configuration -*- lexical-binding: t; -*-

(use-package codeium
  :straight (codeium
             :type git
             :host github
             :repo "Exafunction/codeium.el")
  
  :init
  ;; 将 codeium 添加到 completion-at-point-functions（放在 corfu 之前）
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  
  :config
  ;; 关闭对话框
  (setq use-dialog-box nil)
  
  ;; 降低延迟，使补全更灵敏
  (setq codeium-delay 0.1)
  
  ;; 限制发送给 codeium 的文本长度（提升性能）
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  
  ;; 在 mode-line 显示 codeium 状态
  (setq codeium-mode-line-enable
          (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  
  ;; 限制启用的 APIs（减少不必要的请求）
  (setq codeium-api-enabled
          (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  
  ;; 启动时自动初始化（可选，加速首次使用）
  ;; (add-hook 'emacs-startup-hook
  ;;           (lambda () (run-with-timer 0.1 nil #'codeium-init)))
  )

(provide 'init-codeium)
;;; init-codeium.el ends here
