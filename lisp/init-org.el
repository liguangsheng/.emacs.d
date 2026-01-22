;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Org mode with bullets, todo keywords, and font settings.

;;; Code:

(use-package org-bullets
  :defer t
  :init
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "ABORT(a)"))
        org-todo-keyword-faces '(("TODO" . "red")
                                 ("DOING" . "yellow")
                                 ("DONE" . "green")
                                 ("ABORT" . "blue")))
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode 1)
              ;; Use monospace font for org-table to align borders
              (set-face-attribute 'org-table nil
                                  ;; :family "Noto Sans Mono CJK SC"
                                  :family "Iosevka"
                                  :weight 'normal
                                  :width 'normal))))

(provide 'init-org)

;;; init-org.el ends here
