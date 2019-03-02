(use-package centaur-tabs
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (after-init . centaur-tabs-mode)
  :init
  (setq centaur-tabs-style "bar"
	centaur-tabs-height 32
	centaur-tabs-set-icons t
	;; centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-set-bar 'under
	x-underline-at-descent-line t
	centaur-tabs-set-modified-marker t
	;; centaur-tabs-show-navigation-buttons t
	)
  :config
  (centaur-tabs-change-fonts "arial" 100)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)

  ;; (:map evil-normal-state-map
  ;; 	  ("g t" . centaur-tabs-forward)
  ;; 	  ("g T" . centaur-tabs-backward))
  )

(provide 'init-tabline)
