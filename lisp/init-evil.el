(use-package evil
  :init
  ;; (setq evil-default-cursor        '("#0eadee" box)
  ;; 	evil-normal-state-cursor   '("#0eadee" box)
  ;; 	evil-insert-state-cursor   '("#00cd66" bar)
  ;; 	evil-visual-state-cursor   '("#bebebe" hbar)
  ;; 	evil-replace-state-cursor  '("#0eadee" box)
  ;; 	evil-operator-state-cursor '("#9966cc" box)
  ;; 	evil-motion-state-cursor   '("#d32f2f" box)
  ;; 	evil-emacs-state-cursor    '("#373e40" box))
  :config
  (evil-ex-define-cmd "q" 'kill-this-buffer) ;; make :q just kill buffer, do not exit emacs
  (evil-ex-define-cmd "quit" 'evil-quit)
  (evil-mode 1))

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  )

(provide 'init-evil)
