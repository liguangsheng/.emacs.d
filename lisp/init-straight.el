;;; init-straight.el --- Straight.el package manager -*- lexical-binding: t; ---

;;; Commentary:
;; This file bootstraps Straight.el package manager and configures
;; use-package integration for dependency management.

;;; Code:

(defvar bootstrap-version)
(setq straight-base-dir (expand-file-name "var/" user-emacs-directory))
(let ((bootstrap-file (expand-file-name "var/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package settings before loading
(eval-and-compile
  (setq straight-use-package-by-default t)
  ;; (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(straight-use-package 'use-package)

;; Required packages for use-package
(use-package diminish)
(use-package bind-key)

(provide 'init-straight)

;;; init-straight.el ends here
