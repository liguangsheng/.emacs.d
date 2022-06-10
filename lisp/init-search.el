(use-package wgrep)

(use-package rg)

(use-package color-rg
  :straight '(color-rg :type git
					   :host github
					   :repo "manateelazycat/color-rg")
  :init
  (setq color-rg-mac-load-path-from-shell nil)
  (require 'color-rg)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'color-rg-mode 'insert))
  )

(defun search-current-directory ()
  "Search in current directory with consult-ripgrep."
  (interactive)
  (let ((dir (or default-directory "~/"))
		(initial (if (use-region-p)
					 (buffer-substring-no-properties (region-beginning) (region-end))
				   nil)))
	(consult-ripgrep dir initial)))

(defun search-directory ()
  "Search in specified directory with consult-ripgrep."
  (interactive)
  (let ((dir (read-directory-name "Search directory: "))
		(initial (if (use-region-p)
					 (buffer-substring-no-properties (region-beginning) (region-end))
				   nil)))
	(consult-ripgrep dir initial)))

(defun search-current-project ()
  "Search in current project with consult-ripgrep."
  (interactive)
  (let ((dir (project-root (project-current t)))
		(initial (if (use-region-p)
					 (buffer-substring-no-properties (region-beginning) (region-end))
				   nil)))
	(consult-ripgrep dir initial)))

(defun search-line-in-buffer ()
  "Search line in buffer with consult-line."
  (interactive)
  (let ((initial (if (use-region-p)
					 (buffer-substring-no-properties (region-beginning) (region-end))
				   nil)))
	(consult-line initial)))

(my-leader-def
  "s"  '(:ignore t                :wk "search")
  "sl" '(search-line-in-buffer    :wk "Search line in buffer")
  "sp" '(search-current-project   :wk "Search current project")
  "sd" '(search-current-directory :wk "Search directory")
  "sD" '(search-directory         :wk "Search specified directory")
  "sg" '(vc-git-grep              :wk "Search by git grep")
  "si" '(imenu                    :wk "Search imenu")


  "S"  '(:ignore t                             :wk "color-rg search")
  "Sp" '(color-rg-search-project               :wk "Search project")
  "Sd" '(color-rg-search-input                 :wk "Search specified directory")
  "Sb" '(color-rg-search-input-in-current-file :wk "Search in current file")
  )

(provide 'init-search)
