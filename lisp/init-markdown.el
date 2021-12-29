(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode-hook . (lambda ()
				(set-face-attribute 'markdown-table-face nil 
						    ;; :family "Noto Sans Mono CJK SC"
						    :family "Iosevka"
						    :weight 'normal
						    :width 'normal))))

(provide 'init-markdown)
