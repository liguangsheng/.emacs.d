(use-package all-the-icons)

(setq my-theme (if (display-graphic-p) my-gui-theme my-tui-theme))
;; 字体设置
(when (display-graphic-p)
  (set-face-attribute 'default nil :font my-enfont)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset my-cnfont)))

(use-package doom-themes
  :defer t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; Modeline:
(setq column-number-mode t)
(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar t)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-icon t)
  (doom-modeline-modal-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-number-limit 99)
  (doom-modeline-lsp t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info t)
  (doom-modeline-workspace-name t)
  (doom-modeline-env-go-executable "go")
  (doom-modeline-position-column-format '("C%c"))
  (doom-modeline-position-column-line-format '("L%lC%c"))


  :init
  (doom-modeline-mode 1))


(use-package kaolin-themes)

;; https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :straight (:host github :repo "protesilaos/ef-themes" :files ("*.el")))

(use-package catppuccin-theme
  :init
  (setq catppuccin-flavor 'mocha))

;; 缓存切换的主题到custom-file
(defvar chosen-theme nil)
(defun save-chosen-theme (theme &rest args)
  (customize-save-variable 'chosen-theme theme))
(advice-add 'load-theme :after #'save-chosen-theme)

(cond (chosen-theme (load-theme chosen-theme :no-confirm))
      (*gui*        (load-theme my-gui-theme :no-confirm))
      (t            (load-theme my-tui-theme :no-confirm)))

(provide 'init-theme)
