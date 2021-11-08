;;; init-posframe.el -- posframe ��ص�����

;;; Commentary:

;; posframe相关的配置

;;; Code:

(defvar perferences/enable-posframe t)

(use-package posframe
  :if perferences/enable-posframe)

(use-package ivy-posframe
  :after (:all posframe ivy)
  :init (setq ivy-posframe-border-width 2
	      ivy-posframe-display-functions-alist '((complete-symbol . ivy-posframe-display-at-point)
						     (t . ivy-posframe-display)))
  :config (ivy-posframe-mode 1))

(use-package which-key-posframe
  :after (:all posframe which-key)
  :init (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center
	      which-key-posframe-border-width 2
	      which-key-posframe-parameters '((left-fringe . 5) (right-fringe . 5)))
  :config (which-key-posframe-mode))

;; hydra-posframe 不在melpa里，需要手动从github上下载下来，放到~/.emacs.d/site-lisp/hydra-posframe
;; (use-package hydra-posframe
;;   :if prefer-posframe
;;   :load-path "~/.emacs.d/site-lisp/hydra-posframe"
;;   :hook (after-init . hydra-posframe-mode)
;;   :init
;;   (setq hydra-posframe-border-width 2
;; 	hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center
;; 	hydra-posframe-parameters '((left-fringe . 5)(right-fringe . 5)))
;;   :custom-face (hydra-posframe-border-face ((t (:background "#bf616a"))))
;;   :custom-face (hydra-posframe-face ((t (:background "#3b4252"))))
;;   )

(provide 'init-posframe)
;;; init-posframe.el ends here
