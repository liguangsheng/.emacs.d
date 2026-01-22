(use-package supermaven
  :straight (supermaven
             :type git
             :host github
             :repo "binbandit/supermaven.el")
  :hook
  (prog-mode . supermaven-mode)

  :init
  (setq supermaven-idle-delay 0.1)

  :config
  (setq supermaven-keymaps
        '((accept-suggestion . "TAB")
          (accept-word       . "M-TAB")
          (clear-suggestion  . "C-]")))

  (setq supermaven-ignore-filetypes
        '("org" "markdown" "md" "txt"))

  (setq supermaven-log-level 'info)

  ;; 确保 supermaven 优先级高于 corfu
  (add-hook 'supermaven-mode-hook
            (lambda ()
              (when (bound-and-true-p supermaven-mode)
                (setq-local completion-at-point-functions
                            (cons #'supermaven-completion-at-point
                                  (delete #'supermaven-completion-at-point
                                          completion-at-point-functions)))))))

(provide 'init-supermaven)
