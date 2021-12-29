(defvar preferences/enable-posframe t)

(use-package posframe
  :if preferences/enable-posframe)

(provide 'init-posframe)
