;;; init.el --- Emacs configuration entry -*- lexical-binding: t; -*-

;; Author: Guangsheng Li <ligs.cn@gmail.com>
;; Maintainer: Guangsheng Li <ligs.cn@gmail.com>

;;; Commentary:

;; This file bootstraps the Emacs configuration, which is divided into
;; a number of modular files for better organization and maintainability.
;; It loads core packages, sets up basic configuration, and requires
;; all feature-specific modules.

;;; Code:

;; Performance optimization: Speed up startup by deferring native compilation
(defvar native-comp-deferred-compilation-deny-list nil)

;; Load path configuration
;; Add lisp and opt directories to load-path
(dolist (dir '("opt" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

;; Recursively add opt subdirectories to load-path
(let ((default-directory (expand-file-name "opt" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; Bootstrap package management
(require 'init-elpa)      ; ELPA package configuration
(require 'init-straight)  ; Straight.el package manager
(require 'init-basic)     ; Basic Emacs settings

;;; Customizable Settings

;; User configuration variables
(setq-default
 my-gui-theme            'doom-opera-light  ; Theme for graphical displays
 my-tui-theme            'doom-opera-light  ; Theme for terminal displays
 my-enfont               "MapleMono NF CN:pixelsize=12:weight=medium"  ; English font
 my-cnfont               "LXGW WenKai Mono:pixelsize=14"               ; Chinese font
 my-enable-server        t                    ; Enable Emacs server
 my-auto-restore-session nil                  ; Auto-restore desktop session
 line-spacing            0.2                  ; Line spacing
 )

;;; Core Configuration Modules

;; Load core functionality modules
(require 'init-theme)     ; Theme and appearance
(require 'init-evil)      ; Evil mode (Vim emulation)
(require 'init-keys)      ; Keybindings and leader keys
(require 'init-buffer)    ; Buffer management utilities
(require 'init-window)    ; Window management
(require 'init-fonts)     ; Font configuration

;;; Feature Modules

;; Load feature-specific modules
(require 'init-features)  ; Additional Emacs features and utilities
(require 'init-search)    ; Search and grep functionality
(require 'init-editconfig); EditorConfig support
(require 'init-avy)       ; Fast navigation
(require 'init-project)   ; Project management
(require 'init-flycheck)  ; Syntax checking
(require 'init-dired)     ; File manager enhancements
(require 'init-vcs)       ; Version control (Git, Magit, etc.)
(require 'init-treemacs)  ; File tree explorer
(require 'init-completion); Completion framework (Vertico, Corfu, etc.)
(require 'init-lsp)       ; Language Server Protocol
(require 'init-tags)      ; Ctags and code navigation

;;; Programming Language Modules

;; Load language-specific configurations
(require 'init-prog)      ; General programming modes
(require 'init-go)        ; Go language support
(require 'init-python)    ; Python language support
(require 'init-rust)      ; Rust language support
(require 'init-org)       ; Org mode configuration
(require 'init-markdown)  ; Markdown mode configuration

;;; Experimental Features
;; Add experimental or optional features here

;;; init.el ends here
