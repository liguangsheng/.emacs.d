;; default variables
(defvar gui-theme 'wombat)
(defvar cli-theme 'wombat)

(use-package helm-themes)
(use-package zenburn-theme :defer t)
(use-package dracula-theme :defer t)
(use-package doom-themes :defer t)

(load-theme theme t)

(provide 'init-theme)
;;; init-theme.el ends here

