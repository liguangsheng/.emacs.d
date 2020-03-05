(setq eshell-history-file-name     (concat emacs-cache-dir "eshell-history"))
(setq auto-mode-alist (cons '("\\.bashrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zshrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("_zshrc_mac" . sh-mode) auto-mode-alist))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package shell-pop
  :config
  (setq shell-pop-window-position "right"
	shell-pop-full-span t
	shell-pop-window-size 30))

(use-package aweshell
  :straight (aweshell :type git
		      :host github
		      :repo "manateelazycat/aweshell")
  :config (bind-key "s-/" 'aweshell-toggle))


(provide 'init-shell)
