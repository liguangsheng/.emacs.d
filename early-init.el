;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist) 
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; 最大化Frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; 设置字体
;; (setq-default default-text-properties '(line-spacing 0.0 line-height 19))

;; early-init.el ends here
