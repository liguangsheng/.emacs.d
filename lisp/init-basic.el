(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

(require 'subr-x)
(require 'cl-lib)

(defconst user-emacs-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Path to .emacs.d/lisp directory where init files exists.")

(defconst user-emacs-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Path to .emacs.d/site-lisp directory.")

(defconst user-emacs-etc-directory
  (expand-file-name "etc/" user-emacs-directory))

(defconst user-emacs-var-directory
  (expand-file-name "var/" user-emacs-directory))

(defconst *windows*  (eq system-type 'windows-nt))
(defconst *macos*    (eq system-type 'darwin))
(defconst *linux*    (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *gui*      (display-graphic-p))
(defconst *emacs26+* (>= emacs-major-version 26))
(defconst *emacs27+* (>= emacs-major-version 27))
(defconst *emacs28+* (>= emacs-major-version 28))
(defconst *emacs29+* (>= emacs-major-version 29))

(defun expand-user-etc (path)
  (expand-file-name path user-emacs-etc-directory))

(defun expand-user-var (path)
  (expand-file-name path user-emacs-var-directory))

(defun my-format-buffer ()
  "Format the entire buffer using the `indent-region' function."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun copy-file-path ()
  "Copy the current buffer's file path to the clipboard."
  (interactive)
  (if (buffer-file-name)
      (let ((file-path (buffer-file-name)))
        (kill-new file-path)
        (message "File path '%s' copied to clipboard." file-path))
    (message "Buffer is not visiting a file.")))

(setq keep-alive-buffers '("\\**\\*"
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
  (add-to-list '
   default-frame-alist '(ns-transparent-titlebar . t)))

(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

(use-package no-littering
  :init
  (require 'no-littering))

(use-package so-long
  :config (global-so-long-mode 1))

(use-package whitespace
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face             ; visualize things below:
						   empty            ; empty lines at beginning/end of buffer
						   space-before-tab ; spaces before tab
						   trailing         ; trailing blanks
						   tabs             ; tabs (show by face)
						   ;; tab-mark         ; tabs (show by symbol))))
						   ;; lines-tail       ; lines go beyond `fill-column'
						   )))

(use-package server
  :commands
  (server-running-p server-start)
  :init
  (setq server-socket-dir (expand-user-var "server")
		server-name "server")
  (make-directory server-socket-dir :parents)
  (add-hook 'after-init-hook (lambda () (when (and my-enable-server
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
(setq-default
 auto-mode-case-fold                      nil
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
 custom-file                              (no-littering-expand-etc-file-name "custom.el")
 warning-minimum-level                    :emergency
 bidi-display-reordering                  nil
 clean-buffer-list-delay-special          1
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
(midnight-mode 1)

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


(when (and custom-file (file-readable-p custom-file) (load custom-file)))

(provide 'init-basic)
