(defun init-windows ()
  (message "Using windows config.")
  (setq
   GOROOT "D:\\Tools\\go"
   GOPATH "D:\\Projects\\go"
   GOBIN  (concat GOPATH "\\bin")
   GOEXEC "D:\\Tools\\go\\bin\\go.exe"
   show-menu-bar t
   ))

(defun init-linux ()
  (message "Using linux config.")
  (setq
   GOPATH "~/go"
   GOROOT "/usr/lib/go"
   GOBIN (concat GOPATH "/bin")
   GOEXEC "go"
   english-font '("Ubuntu Mono" 14)
   ))

(defun init-mac ()
  (message "Using mac-os config.")
  (setq
   GOPATH "/Users/guangshengli/lls"
   GOROOT "/usr/local/go"
   GOBIN  (concat GOPATH "/bin")
   )
  (add-to-list 'exec-path "/usr/local/Cellar/global/6.6.2_1/bin")
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>")  'end-of-line)

  ;;(use-package exec-path-from-shell
  ;;  :config
  ;;  (exec-path-from-shell-initialize))
  )

(cond
 ((windows-p) (init-windows))
 ((linux-p) (init-linux))
 ((mac-os-p) (init-mac)))

(provide 'init-env)
