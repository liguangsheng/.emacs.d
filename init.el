;; init.el --- Emacs configuration entry -*- lexical-binding: t; -*- ;;
;; Author: Guangsheng Li <ligs.cn@gmail.com>
;; Maintainer: Guangsheng Li <ligs.cn@gmail.com>

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Speed up startup

;; Add dir to load-path
(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

;; Recursive add site-lisp to load-path
(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

(defvar native-comp-deferred-compilation-deny-list nil)

(require 'init-elpa)
(require 'init-straight)
(require 'init-basic)

;;; Quick Settings:

(setq-default
 my-gui-theme            'doom-opera-light
 my-tui-theme            'doom-opera-light
 my-enfont               "MapleMono NF CN:pixelsize=12:weight=medium"
 my-cnfont               "LXGW WenKai Mono:pixelsize=14"
 my-enable-server        t
 my-auto-restore-session nil
 line-spacing            0.2
 )

;; core
(require 'init-theme)
(require 'init-evil)
(require 'init-keys)
(require 'init-buffer)
(require 'init-window)
(require 'init-fonts)

;; features
(require 'init-features)
(require 'init-search)
(require 'init-editconfig)
(require 'init-avy)
(require 'init-project)
(require 'init-flycheck)
(require 'init-dired)
(require 'init-vcs)
(require 'init-treemacs)
(require 'init-completion)
;; (require 'init-copilot)
;; (require 'init-codeium)
;; (require 'init-supermaven)
(require 'init-lsp)
(require 'init-tags)
;; (require 'init-lsp-bridge)

;; programing languages
(require 'init-prog)
(require 'init-go)
(require 'init-python)
(require 'init-rust)
(require 'init-org)
(require 'init-markdown)

;; exp

(;;; Init.el ends here
