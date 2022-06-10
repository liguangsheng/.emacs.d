(use-package project
  :config
  (my-leader-def "p" '(:keymap project-prefix-map :package project))
  )

(provide 'init-project)
