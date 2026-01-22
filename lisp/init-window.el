;;; init-window.el --- Window management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures window management utilities including zoom-window
;; and a hydra for window operations.

;;; Code:

;; Zoom window package for toggling window zoom
(use-package zoom-window)

;; Pretty hydra for window management commands
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
    ("n" zoom-window-next "zoom window next"))))

;; Leader key binding to invoke window hydra
(my-leader-def "w" 'window-hydra/body)

(provide 'init-window)

;;; init-window.el ends here
