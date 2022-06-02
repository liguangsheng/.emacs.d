(defvaralias 'preferences/python-executable 'python-shell-interpreter)

(defvar preferences/enable-server nil)

(defun copy-file-path ()
  (interactive "P")
  (kill-new (file-relative-name  (buffer-file-name) (projectile-project-root))))

(defun format-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(setq-default keep-alive-buffers '("\\**\\*"
				   "^\\*scratch\\*$"
				   "^\\*Messages\\*$"
				   ))

(defun kill-all-buffers ()
  (interactive)
  (cl-loop for buffer in (buffer-list)
	   for bufname = (s-trim (buffer-name buffer))
	   unless (cl-loop for rx in keep-alive-buffers
			   when (> (s-count-matches rx bufname) 0)
			   return bufname)
	   do (kill-buffer bufname)))

(defun kill-other-buffers ()
  (interactive)
  (cl-loop for buffer in (buffer-list)
	   for bufname = (buffer-name buffer)
	   unless (or (string= bufname (buffer-name (current-buffer)))
		      (cl-loop for rx in keep-alive-buffers
			       when (> (s-count-matches rx bufname) 0)
			       return bufname))
	   do (kill-buffer bufname)))

(defun switch-to-modified-buffer ()
  "Switch to modified buffer"
  (interactive)
  (let ((buf-list
	 (seq-filter
	  (lambda (x) (not (or
			    (not (buffer-modified-p x))
			    (s-prefix? "*" (buffer-name x))
			    (s-prefix? " *" (buffer-name x))
			    (s-suffix? "-mode" (buffer-name x)))))
	  (buffer-list))))
    (if buf-list
	(switch-to-buffer (first buf-list))
      (message "No modified buffer."))))

(defun open-init-el ()
  "Open ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-inbox ()
  "Open ~/INBOX"
  (interactive)
  (find-file "~/INBOX"))

(defun switch-to-scratch ()
  "Swtich to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun random-choice (LIST)
  "Get a random choice from LIST"
  (nth (mod (random) (length LIST)) LIST))

(defmacro withf (func &rest body)
  (declare (indent 1) (debug t))
  `(when (fboundp ,func) ,@body))

(defun add-hooks (hooks functions)
  (let ((hooks (ensure-list hooks))
	(functions (if (functionp functions) (list functions) functions)))
    (dolist (hook hooks)
      (dolist (func functions)
	(add-hook hook func)))))

(when (< emacs-major-version 27)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(use-package no-littering
  :init
  (require 'no-littering))

(use-package so-long
  :config (global-so-long-mode 1))

(use-package whitespace
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face             ; visualize things below:
			   ;; empty            ; empty lines at beginning/end of buffer
			   ;; lines-tail       ; lines go beyond `fill-column'
			   space-before-tab ; spaces before tab
			   trailing         ; trailing blanks
			   tabs             ; tabs (show by face)
			   ;; tab-mark         ; tabs (show by symbol))))
			   )))

(use-package server
  :commands
  (server-running-p server-start)
  :init
  (setq server-socket-dir (expand-user-var "server")
	server-name "server")
  (make-directory server-socket-dir :parents)
  (add-hook 'after-init-hook (lambda () (when (and preferences/enable-server
						   (not (server-running-p)) (server-start)))))
  :config
  (defun server-ensure-safe-dir (dir) "Noop" t)
  )

(use-package gcmh
  :diminish
  :init
  (setq gcmh-idle-delay 5
	gcmh-verbose nil
	gcmh-high-cons-threshold #x6400000) ;; 100 MB
  (gcmh-mode 1))

;; Better variables
(setq
 apropos                                  t
 backup-by-copying                        t
 comint-prompt-read-only                  t
 compilation-always-kill                  t
 compilation-ask-about-save               nil
 compilation-scroll-output                t
 create-lockfiles                         nil
 debug-on-error                           t
 delete-old-versions                      t
 font-lock-maximum-size                   5000000
 history-length                           1024
 idle-update-delay                        0.5
 inhibit-startup-message                  t
 kept-new-versions                        6
 kept-old-versions                        2
 large-file-warning-threshold             100000000
 tab-width                                4
 vc-follow-symlinks                       t
 version-control                          t
 visible-bell                             0
 inhibit-compacting-font-caches           t
 scroll-conservatively                    1000
 scroll-margin                            10
 help-window-select                       t
 )

;; Backup
(setq  make-backup-files nil
       backup-inhibited  t
       auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)
;; (cua-mode 1)
;; (horizontal-scroll-bar-mode -1)
(global-auto-revert-mode 1)
(setq recentf-max-saved-items 1000)
(recentf-mode 1)
(ignore-errors (savehist-mode 1))
(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; 高亮当前行
(global-hl-line-mode 1)

;;; Line Number:
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; 全局开启折行模式
(set-default 'truncate-lines t)
(setq-default truncate-partial-width-windows nil)
(global-visual-line-mode t)

;; Use utf-8 as default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))               ; pretty
(prefer-coding-system                   'utf-8)  ; pretty
(set-terminal-coding-system             'utf-8)  ; pretty
(set-keyboard-coding-system             'utf-8)  ; pretty
(set-selection-coding-system            'utf-8)  ; pretty
(setq locale-coding-system              'utf-8)  ; pretty
(setq-default buffer-file-coding-system 'utf-8)  ; with sugar on top

;; Chinese encoding for windows
(when (eq system-type 'windows-nt)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))


;; (defvar toggle-one-window-window-configuration nil
;;   "The window configuration use for `toggle-one-window'.")

;; (defun toggle-one-window ()
;;   "Toggle between window layout and one window."
;;   (interactive)
;;   (if (equal (length (cl-remove-if #'window-dedicated-p (window-list))) 1)
;;       (if toggle-one-window-window-configuration
;; 	  (progn
;; 	    (set-window-configuration toggle-one-window-window-configuration)
;; 	    (setq toggle-one-window-window-configuration nil))
;; 	(message "No other windows exist."))
;;     (setq toggle-one-window-window-configuration (current-window-configuration))
;;     (delete-other-windows)))

;; Windows下的版本可能没有这个函数，兼容一下
(unless (functionp 'ensure-list)
  (defun ensure-list (object)
    (if (listp object)
	object
      (list object))))

(provide 'init-basic)
