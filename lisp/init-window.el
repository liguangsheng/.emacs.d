(use-package zoom-window)

(pretty-hydra-define window-hydra
  (:color teal :quit-key "q" :title "Window Management")
  ("Window"
   (("x" delete-window "delete focused")
	("da" ace-delete-window "delete by ace")
	("do" delete-other-windows "delete other windows")
	("s" ace-swap-window "swap window"))
   "Jump"
   (("w" ace-window "ace-window")
	("j" next-window-any-frame "next-window")
	("k" previous-window-any-frame "prev-window"))
   "Split"
   (("|" split-window-horizontally "split horizontal")
	("-" split-window-vertically   "split vertically"))
   "Zoom"
   (("m" zoom-window-zoom "zoom window")
	("n" zoom-window-next "zoom window next"))
   ))

(my-leader-def "w" 'window-hydra/body)

(provide 'init-window)
