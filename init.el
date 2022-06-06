;; init.el --- Emacs configuration entry -*- lexical-binding: t; -*- ;;
;; Author: Guangsheng Li <ligs.cn@gmail.com>
;; Maintainer: Guangsheng Li <ligs.cn@gmail.com>

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(require 'init-startup (expand-file-name "lisp/init-startup.el" user-emacs-directory))

;;; Quick Settings:

(setq-default
 preferences/theme  (if *gui* 'doom-dracula nil)
 preferences/font   "JetBrainsMonoMedium Nerd Font:pixelsize=12:weight=medium"
 preferences/cnfont (font-spec :family "AR PL UKai CN" :size 14)
 preferences/python-executable (cond (*windows* "C:\\Program Files\\Python39\\python.exe")
				     (t         "python3"))
 preferences/enable-server (not *windows*)
 line-spacing 0.3
 )

;; core
;; (require 'init-elpa)
(require 'init-straight)
(require 'init-basic)
(require 'init-fonts)
(require 'init-theme)
(require 'init-keys)
(require 'init-projectile)

;; languages & major-modes
(require 'init-go)
(require 'init-python)
(require 'init-rust)
(require 'init-org)
(require 'init-markdown)
(require 'init-langs)

;; editing
(require 'init-evil)
(require 'init-search)
(require 'init-editconfig)

;; tools
(require 'init-features)
(require 'init-posframe)
(require 'init-icons)
(require 'init-tabline)
(require 'init-tags)
(require 'init-completion)
;; (require 'init-company)
;; (require 'init-lsp)
(require 'init-corfu)
(require 'init-lsp-bridge)
(require 'init-flycheck)
(require 'init-dired)
(require 'init-vcs)
(require 'init-hydra)
(require 'init-treemacs)
(require 'init-tree-sitter)

;;; Experimental:

;;; Load custom file:
(when (and custom-file (file-readable-p custom-file) (load custom-file)))

;;; init.el ends here
