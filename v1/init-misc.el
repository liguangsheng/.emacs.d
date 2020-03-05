(use-package window-numbering)

(use-package fancy-battery
  :config (add-hook 'after-init-hook #'fancy-battery-mode))

(use-package awesome-tab
  :straight (awesome-tab
	     :type git
	     :host github
	     :repo "manateelazycat/awesome-tab")
  :config
  (setq awesome-tab-style 'slant
	awesome-tab-cycle-scope 'tabs)
  (awesome-tab-mode t)
  :bind* (("s-<left>"  . awesome-tab-backward)
	  ("s-<right>" . awesome-tab-forward)
	  ("s-<up>"    . awesome-tab-forward-group)
	  ("s-<down>"  . awesome-tab-backward-group))
  )

(use-package format-all)

(use-package json-navigator)

(provide 'init-misc)
