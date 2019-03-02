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
 perferences/theme (cond (*windows* (if *gui* 'doom-spacegrey 'nil))
			 (*macos*   (if *gui* 'doom-spacegrey 'doom-spacegrey))
			 (*linux*   (if *gui* 'doom-spacegrey 'doom-spacegrey)))

 perferences/font   "Roboto Mono:pixelsize=14"
 perferences/cnfont (font-spec :family "WenQuanYi Micro Hei" :size 14)

 perferences/python-executable (cond (*windows* "C:\\Program Files\\Python39\\python.exe")
				     (t         "python3"))

 perferences/enable-icons    *gui*
 perferences/enable-tabnine  t
 perferences/enable-posframe nil
 perferences/enable-server   t
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
(require 'init-emoji)

;; editing
(require 'init-evil)

;; modules
(require 'init-features)
(require 'init-dired)
(require 'init-vcs)
(require 'init-hydra)
(require 'init-treemacs)
(require 'init-completion)

;; tools
(require 'init-lsp)
(require 'init-editconfig)

;; keys
;; (require 'init-keybindings)

;; languages
(require 'init-lang)

;;; Experimental:

;;; Load custom file:
(when (and custom-file (file-readable-p custom-file) (load custom-file)))

;;; init.el ends here
