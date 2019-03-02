;;; init-packages.el -- packages configuration

;;; Commentary:

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name ".local/elpa" user-emacs-directory))
(setq package-check-signature nil)
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")))
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; ;; Update GPG keyring for GNU ELPA
;; (use-package gnu-elpa-keyring-update)

;; ;; Auto update packages
;; (use-package auto-package-update
;;   :init
;;   (setq auto-package-update-delete-old-versions t
;; 	auto-package-update-hide-results t)
;;   (defalias 'upgrade-packages #'auto-package-update-now))

;; init `straight.el'
(defvar bootstrap-version)
(setq straight-base-dir (expand-dotlocal ""))
(let ((bootstrap-file    (expand-dotlocal "straight/repos/straight.el/bootstrap.el"))
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

(provide 'init-packages)
;;; init-packages.el ends here
