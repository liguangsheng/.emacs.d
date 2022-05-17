;;; init-c.el --- <Summary>
;;; Commentary:
;;  Require ccls
;;; Code:

(use-package ccls
  :defer t
  :defines projectile-project-root-files-top-down-recurring
  :init
  (setq ccls-initialization-options '(:index (:comments 2)
					     :completion (:detailedLabel t)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
	  (append '("compile_commands.json"
		    ".ccls")
		  projectile-project-root-files-top-down-recurring))))

(provide 'init-c)
;;; init-c.el ends here
