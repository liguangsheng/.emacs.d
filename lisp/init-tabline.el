(use-package centaur-tabs
  :hook
  (dired-mode . centaur-tabs-local-mode)
  :init
  (setq centaur-tabs-style "chamfer"
	centaur-tabs-height 32
	centaur-tabs-set-icons t
	centaur-tabs-plain-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-set-bar 'left
	centaur-tabs-set-modified-marker t)
  ;; (centaur-tabs-change-fonts "arial" 160)
  :config (centaur-tabs-mode t)
  )


(provide 'init-tabline)
