;;; Commentary:
;; init-core.el

;;; Code:

(defvar emacs-root-dir (file-truename user-emacs-directory)
  "Path to .emacs.d directory")

(defvar emacs-module-dir  (expand-file-name "modules/" emacs-root-dir)
  "Path to .emacs.d/lisp directory where init files exists.")

(defvar emacs-lisp-dir  (expand-file-name "lisp/" emacs-root-dir)
  "Path to .emacs.d/lisp directory where init files exists.")

(defvar emacs-lisp-lang-dir  (expand-file-name "lisp/lang/" emacs-root-dir)
  "Path to .emacs.d/lisp/lang directory")

(defvar emacs-site-lisp-dir (expand-file-name "site-lisp/" emacs-root-dir)
  "Path to .emacs.d/site-lisp directory.")

(defvar emacs-cache-dir (expand-file-name ".cache/" emacs-root-dir)
  "Path to cache directory.")

(defvar emacs-etc-dir (expand-file-name "etc/" emacs-root-dir)
  "Path to etc directory.")

(defun ensure-dir (DIR)
  (if (not (file-exists-p DIR)) (make-directory DIR)))

(ensure-dir emacs-cache-dir)
(ensure-dir emacs-etc-dir)

;; add lisp and core dir to load-path
(add-to-list 'load-path emacs-lisp-dir)
(add-to-list 'load-path emacs-lisp-lang-dir)


;; add site-lisp to load-path
(let ((default-directory emacs-site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))


(provide 'init-core)
;;; core.el ends here
