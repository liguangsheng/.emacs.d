(use-package project
  :init
  (setq project-vc-extra-root-markers '(".git" ".svn" ".hg" ".bzr" "_darcs" "go.mod"))
  :config
  (my-leader-def "p" '(:keymap project-prefix-map :package project)
	"pg" #'search-current-project))

(provide 'init-project)
