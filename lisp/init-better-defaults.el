;;; Code:

(require 'cl-lib)

(defvar show-menu-bar-p nil
  "Show menu bar when show-menu-bar-p is t.")
(if show-menu-bar-p (menu-bar-mode 1) (menu-bar-mode -1))

(defvar show-tool-bar-p nil "show tool bar when show-tool-bar-p is t")
(if show-tool-bar-p (tool-bar-mode 1) (tool-bar-mode 0))

(defvar blink-cursor-p nil "blink cursor when blink-cursor-p is t")
(if blink-cursor-p (blink-cursor-mode 1) (blink-cursor-mode 0))

(defvar show-scroll-bar-p nil "show scroll bar when show-scroll-bar-p is t")
(if (and (display-graphic-p) show-scroll-bar-p) (scroll-bar-mode 1) (scroll-bar-mode 0))

(defvar maximize-frame-at-start-p t "maximize-frame-at-start-p")
(when maximize-frame-at-start-p (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(defvar show-line-number-p nil "show line number")
(when show-line-number-p (linum-mode))

;; use y/n insteal of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use utf-8 as default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))               ; pretty
(prefer-coding-system                   'utf-8)  ; pretty
(set-terminal-coding-system             'utf-8)  ; pretty
(set-keyboard-coding-system             'utf-8)  ; pretty
(set-selection-coding-system            'utf-8)  ; pretty
(setq locale-coding-system              'utf-8)  ; pretty
(setq-default buffer-file-coding-system 'utf-8)  ; with sugar on top

;; Better variables
(setq vc-follow-symlinks t)
(setq apropos                      t
      comint-prompt-read-only      t
      compilation-always-kill      t
      compilation-ask-about-save   nil
      compilation-scroll-output    t
      debug-on-error               t
      gc-cons-threshold            50000000
      history-length               1000
      idle-update-delay            2
      large-file-warning-threshold 100000000
      visible-bell                 0
      ;; backup
      backup-by-copying            t
      delete-old-versions          t
      kept-new-versions            6
      kept-old-versions            2
      version-control              t
      ;; files
      auto-save-list-file-name     (concat emacs-cache-dir "autosave/")
      backup-directory-alist       `(("." . ,(concat emacs-cache-dir "backups")))
      nsm-settings-file            (concat emacs-cache-dir "network-security.data")
      recentf-save-file            (concat emacs-cache-dir "recentf")
      server-auth-dir              (concat emacs-cache-dir "server/")
      tramp-auto-save-directory    (concat emacs-cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist backup-directory-alist
      tramp-persistency-file-name  (concat emacs-cache-dir "tramp-persistency.el")
      )

;; 将custom.el移到etc目录
(setq custom-file (concat emacs-etc-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; 去除启动画面
(setq inhibit-startup-message t)

;; 自动重新加载buffer
(auto-revert-mode)

(provide 'init-better-defaults)
