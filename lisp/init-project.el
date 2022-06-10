(use-package project
  :config
  (my-leader-def "p" '(:keymap project-prefix-map :package project)
	"pg" #'search-current-project))

(provide 'init-project)
