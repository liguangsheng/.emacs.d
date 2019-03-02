
;; Speed up startup
(setq auto-mode-case-fold nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

(require 'subr-x)
(require 'cl-lib)

(defconst user-emacs-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Path to .emacs.d/lisp directory where init files exists.")

(defconst user-emacs-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Path to .emacs.d/site-lisp directory.")

(defconst user-emacs-dotlocal-directory
  (expand-file-name ".local/" user-emacs-directory))

;; Add dir to load-path
(add-to-list 'load-path user-emacs-lisp-directory)
(add-to-list 'load-path user-emacs-site-lisp-directory)

;; Recursive add site-lisp to load-path
(let ((default-directory user-emacs-site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))

(defconst *windows* (eq system-type 'windows-nt))
(defconst *macos* (eq system-type 'darwin))
(defconst *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *gui* (display-graphic-p))
(defconst *emacs26+* (>= emacs-major-version 26))
(defconst *emacs27+* (>= emacs-major-version 27))
(defconst *emacs28+* (>= emacs-major-version 28))

(defun expand-dotlocal (path)
  (expand-file-name path user-emacs-dotlocal-directory))

(provide 'init-startup)
