;;; init-straight.el --- 
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(setq straight-base-dir (expand-user-var""))
(let ((bootstrap-file    (expand-user-var "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; init `use-package'
;; Should set before loading `use-package'
(eval-and-compile
  (setq straight-use-package-by-default t)
  ;;  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(straight-use-package 'use-package)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

(provide 'init-straight)
;;; init-straight.el ends here
