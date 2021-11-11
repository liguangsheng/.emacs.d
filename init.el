;; init.el --- Emacs configuration -*- lexical-binding: t; -*- ;;
;; Author: Guangsheng Li <ligs.cn@gmail.com>
;; Maintainer: Guangsheng Li <ligs.cn@gmail.com>

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(require 'init-startup (expand-file-name "lisp/init-startup.el" user-emacs-directory))

;;; Quick Settings:

(setq-default
 perferences/theme (cond (*windows* (if *gui* 'doom-spacegrey 'nil))
			 (*macos*   (if *gui* 'doom-spacegrey 'doom-spacegrey))
			 (*linux*   (if *gui* 'doom-spacegrey 'doom-spacegrey)))

 perferences/font   "Roboto Mono:pixelsize=14"
 perferences/cnfont (font-spec :family "WenQuanYi Micro Hei" :size 14)

 perferences/python-executable (cond (*windows* "C:\\Program Files\\Python39\\python.exe")
				     (t         "python3"))

 perferences/enable-icons    *gui*
 perferences/enable-tabnine  t
 perferences/enable-posframe t
 perferences/enable-server   t

 line-spacing 0.0
 )

;; core
(require 'init-packages)
(require 'init-basic)
(require 'init-fonts)
(require 'init-theme)
(require 'init-projectile)

;; ui
(require 'init-icons)
(require 'init-posframe)
(require 'init-tabline)
(require 'init-keys)

;; editor
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; modules
(require 'init-features)
(require 'init-dired)
(require 'init-vcs)
(require 'init-evil)
(require 'init-hydra)
(require 'init-treemacs)
(require 'init-completion)

;; tools
(require 'init-lsp)

;; keys
;; (require 'init-keybindings)

;; languages
(require 'init-lang)

;;; Experimental:

;;; Load custom file:
(when (and custom-file (file-readable-p custom-file) (load custom-file)))

;;; init.el ends here
