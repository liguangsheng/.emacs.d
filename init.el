;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

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
 blink-cursor-p nil
 ;; 平滑滚动
 smooth-scrolling-p t
 ;; 中英文字体
 ;; https://github.com/powerline/fonts
 ;; curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
 en-fonts '("Fira Mono for Powerline" 13 "Source Code Pro" 13 "Courier New" 13)
 cn-fonts '("华文细黑" 16 "宋体" 15 "微软雅黑" 15)
 ;; 默认主题
 theme 'doom-one
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
(require 'init-misc)
(require 'init-editing)
(require 'init-helm)
(require 'init-theme)
(require 'init-font)
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
(require 'init-markdown)
(require 'init-protocol)
(require 'init-go)
(require 'init-rust)
(require 'init-lua)
(require 'init-typescript)
(require 'init-bazel)

;;; init.el ends here
