;;; init-linum.el --- Line number configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Feature: line number
(defvar show-line-number? nil)
(defvar show-relative-linum? nil)

(use-package nlinum
  :init
  (setq nlinum-highlight-current-line t))

(use-package nlinum-relative
  :init
  (defun turn-on-relative-mode ()
    (interactive)
    (nlinum-relative-on))

  (defun turn-off-relative-mode ()
    (interactive)
    (nlinum-relative-off)
    (linum-mode -1))

  (defun toggle-relative-mode ()
    (interactive)
    (setq show-relative-linum? (not show-relative-linum?))
    (if show-relative-linum? (turn-on-relative-mode)
      (turn-off-relative-mode)))

  (setq nlinum-relative-redisplay-delay 0
	nlinum-relative-current-symbol "-->"
	nlinum-relative-offset 0)
  :config
  (cond
   (show-relative-linum? (turn-on-relative-mode))
   (show-line-number? (global-nlinum-mode)))
  )


(provide 'init-linum)
;;; init-linum.el ends here
