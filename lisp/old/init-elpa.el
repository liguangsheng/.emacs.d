(require 'cl)
(require 'package)

(setq package-archives
      '(
        ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ("org-cn"   . "http://elpa.emacs-china.org/org/")
        ; ("popkit"   . "http://elpa.popkit.org/packages/")

        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("org"   . "http://orgmode.org/elpa/")
        ;;("gnu"    . "http://elpa.gnu.org/packages/")
        ))

(setq package-enable-at-startup nil)
(package-initialize)

(defun packages-installed-p (packages)
  "Return t if all package in PACKAGES installed else nil."
  (every #'package-installed-p packages))

(defun install-package (package)
  "Install package if PACKAGE is not installed."
    (message (format "downloading package %s" package))
    (package-refresh-contents)
    (package-install package))

(defun install-packages (packages)
  (mapc #'install-package packages))

(defun require-package (package)
  (unless (package-installed-p package)
    (install-package package)))

(defun require-packages (packages)
  (mapc #'require-package packages))

;; Init use-package
(require-package 'use-package)
(eval-when-compile
  (require 'use-package))
(require-package 'diminish)                ;; if you use :diminish
(require-package 'bind-key)                ;; if you use any :bind variant
(setq use-package-always-ensure t)

(provide 'init-elpa)

