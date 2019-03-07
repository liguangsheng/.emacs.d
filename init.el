;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;;
;; all-the-icons requirements:
;; M-x all-the-icons-install-fonts
;; 
;; tabnine requirements:
;; M-x company-tabnine-install-binary

;;; Code:

(setq-default
 ;; 是否显示菜单栏
 show-menu-bar-p t
 ;; 是否显示工具栏
 show-tool-bar-p nil
 ;; 显示行号
 show-line-number-p nil
 ;; 滚动条
 show-scroll-bar-p nil
 ;; 启动时窗口最大化
 maximize-frame-at-start-p t
 ;; 是否启用光标闪烁
 blink-cursor-p t
 ;; 平滑滚动
 smooth-scrolling-p t
 ;; 中英文字体
 ;; https://github.com/powerline/fonts
 ;; curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
 en-fonts '("Fira Mono for Powerline" 13 "Source Code Pro" 13 "Courier New" 13)
 cn-fonts '("华文细黑" 16 "宋体" 15 "微软雅黑" 15)
 ;; 默认主题，如果是列表，则随机选择一项加载
 theme 'doom-one
 ;; python
 python-shell-interpreter "/usr/local/bin/python3"
 ;; 是否使用lsp作为python补全后端
 python-lsp-p t
 ;; 是否启动emacs server
 server-p t
 server-socket-dir "/tmp/emacs-server/"
 server-name "emacs-server"
 )

;; 不同平台差异化配置
(if (eq system-type 'windows-nt)
    (setq-default
     en-fonts '("Source Code Pro" 13 "Courier New" 13)
     cn-fonts '("华文细黑" 16 "宋体" 15 "微软雅黑" 15)
     server-p nil
     maximize-frame-at-start-p nil
     ))

;; core
(require 'init-core (concat user-emacs-directory "lisp/init-core"))
(require 'init-better-defaults)
(require 'init-straight)
(require 'init-font)
(require 'init-evil)
(require 'init-editing)
(require 'init-font)
(require 'init-theme)
(require 'init-helm)
(require 'init-highlight)
(require 'init-shell)
(require 'init-modeline)
(require 'init-completion)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-treemacs)
(require 'init-server)
(require 'init-lsp)
(require 'init-keybindings)

;; lang
(require 'init-org)
(require 'init-protocol)
(require 'init-go)
(require 'init-typescript)

;; ;; minor mode & utils
;; (require 'init-server)
;; (require 'init-misc)
;; (require 'init-dashboard)
;; (require 'init-file-manager)
;; (require 'init-prettify)
;; (require 'init-tags)
;; (require 'init-lsp)
;; (require 'init-restart)
;; (require 'init-highlight)
;; (require 'init-pairs)
;; (require 'init-git)
;; (require 'init-smooth-scrolling)
;; (require 'init-linum)
;; (require 'init-quickrun)
;; (require 'init-project)
;; (require 'init-flycheck)
;; (require 'init-completion)

;; ;; programing mode
;; (require 'init-cc)
;; (require 'init-shell)
;; (require 'init-markdown)
;; (require 'init-protobuf)
;; (require 'init-python)
;; (require 'init-ruby)
;; (require 'init-go)
;; (require 'init-lua)
;; (require 'init-web)
;; (require 'init-lisp)
;; (require 'init-javascript)
;; (require 'init-yaml)
;; (require 'init-org)
;; (require 'init-haskell)
;; (require 'init-bazel)
;; (require 'init-rust)

;;; init.el ends here
