;; init.el --- Emacs configuration -*- lexical-binding: t; -*- ;;
;; Author: Guangsheng Li <ligs.cn@gmail.com>
;; Maintainer: Guangsheng Li <ligs.cn@gmail.com>

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(require 'init-startup (expand-file-name "lisp/init-startup.el" user-emacs-directory))
(require 'init-packages)

;;; Quick Settings:

(setq-default
 perferences/theme (cond (*windows* (if *gui* 'doom-spacegrey 'base16-nord))
			 (*macos*   (if *gui* 'doom-nord 'doom-nord))
			 (*linux*   (if *gui* 'doom-nord 'doom-one)))

 perferences/font   "Roboto Mono:pixelsize=12"
 perferences/cnfont (font-spec :family "WenQuanYi Micro Hei" :size 14)

 perferences/python-executable (cond (*windows* "C:\\Program Files\\Python39\\python.exe")
				     (t         "python3"))

 perferences/enable-icons    *gui*
 perferences/enable-tabnine  t
 perferences/enable-posframe t
 perferences/enable-server   t

 line-spacing       0.12
 )


(require 'init-basic)

;; Initialize UI
(require 'init-fonts)
(require 'init-theme)
(require 'init-icons)
(require 'init-posframe)

(require 'init-features)
(require 'init-dired)
(require 'init-evil)
(require 'init-hydra)
(require 'init-treemacs)
(require 'init-ivy)
(require 'init-company)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-keybindings)
(require 'init-lang)

;;; Experimental:
(use-package esup)
(message (format "%d packages loaded in %s" (length package-alist) (emacs-init-time)))

;;; Load custom file:
(when (and custom-file (file-readable-p custom-file) (load custom-file)))

;;; init.el ends here
