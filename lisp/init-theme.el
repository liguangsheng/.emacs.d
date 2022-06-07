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
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; Modeline:
(use-package doom-modeline
  :init
  (setq doom-modeline-height 25
	doom-modeline-bar t
	doom-modeline-bar-width 6
	doom-modeline-hud nil
	doom-modeline-enable-word-count t
	doom-modeline-window-width-limit fill-column
	doom-modeline-project-detection 'project
	doom-modeline-buffer-file-name-style 'relative-to-project
	doom-modeline-icon t
	doom-modeline-modal-icon t
	doom-modeline-major-mode-icon t
	doom-modeline-buffer-state-icon t
	doom-modeline-buffer-modification-icon t
	doom-modeline-number-limit 99
	doom-modeline-lsp t
	doom-modeline-enable-word-count t
	doom-modeline-buffer-encoding t
	doom-modeline-indent-info t
	doom-modeline-workspace-name t
	doom-modeline-env-go-executable "go")
  (doom-modeline-mode 1)
  )

(use-package atom-one-dark-theme  :straight (atom-one-dark-theme  :type git :host github :repo "jonathanchu/atom-one-dark-theme"))
(use-package atom-one-light-theme :straight (atom-one-light-theme :type git :host github :repo "jonathanchu/atom-one-light-theme"))
(use-package uwu-theme :straight (uwu-theme :type git :host github :repo "kborling/uwu-theme.el"))
(use-package kaolin-themes)
(use-package modus-themes)

(defun switch-to-light-theme () (interactive) (load-theme preferences/light-theme t))
(defun switch-to-dark-theme  () (interactive) (load-theme preferences/dark-theme  t))

(when *gui* (switch-to-dark-theme))

(provide 'init-theme)
