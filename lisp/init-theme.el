;;; init-theme.el -- 主题相关配置

;;; Commentary:

;;; Code:

(use-package doom-themes
  :defer t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; Modeline:
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config  (setq doom-modeline-height 25
		 doom-modeline-bar t
		 doom-modeline-bar-width 10
		 doom-modeline-hud nil
		 doom-modeline-window-width-limit fill-column
		 doom-modeline-project-detection 'project
		 doom-modeline-buffer-file-name-style 'relative-to-project
		 doom-modeline-icon (display-graphic-p)
		 doom-modeline-modal-icon nil
		 doom-modeline-major-mode-icon t
		 doom-modeline-buffer-state-icon t
		 doom-modeline-buffer-modification-icon t
		 doom-modeline-number-limit 99
		 doom-modeline-lsp t
		 doom-modeline-enable-word-count t
		 doom-modeline-buffer-encoding t
		 doom-modeline-indent-info t
		 doom-modeline-workspace-name t
		 doom-modeline-env-go-executable "go"))

(use-package atom-one-dark-theme :defer t)

(add-hook 'emacs-startup-hook (lambda () (load-theme 'doom-acario-light t)))


(provide 'init-theme)
;;; init-theme.el ends here
