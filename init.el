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
 my-gui-theme            'doom-nord
 my-tui-theme            'doom-nord
 my-enfont               "JetBrainsMono Nerd Font:pixelsize=14:weight=medium"
 my-cnfont               "LXGW WenKai Mono:pixelsize=16"
 my-enable-server        t
 my-auto-restore-session nil
 line-spacing            0.0
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
(require 'init-copilot)
(require 'init-completion)
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
(defun close-idle-buffers ()
  "Close buffers that have been open for more than 5 minutes and have not been edited.
Skip buffers with names like *Message*."
  (interactive)
  (let* ((current-time (current-time))
         (cutoff-time (time-subtract current-time (seconds-to-time (* 5 60))))
         (buffers-to-close '()))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (unless (string-match-p "\\*\\(.*\\)\\*" (buffer-name buffer))
          ;; Skip buffers with names like *Message*
          (let* ((buffer-open-time (or buffer-display-time (current-time)))
                 (duration (time-to-seconds (time-subtract current-time buffer-open-time)))
                 (duration-minutes (/ duration 60)))
            (message "Buffer %s has been open for %.2f minutes"
                     (buffer-name buffer)
                     duration-minutes))
          (when (and (not (buffer-modified-p))
                     (time-less-p buffer-display-time cutoff-time))
            (push buffer buffers-to-close)))))
    (dolist (buffer buffers-to-close)
      (message "Closing buffer %s" (buffer-name buffer))
      (kill-buffer buffer))
    (message "Closed %d idle buffers. Remaining %d buffers" (length buffers-to-close) (length (buffer-list)))))

(;;; init.el ends here
