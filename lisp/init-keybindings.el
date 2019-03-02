;;; init-keybindings.el -- keybindings configuration

;;; Commentary:

;;; Code:

;; function aliases
(defalias 'my-switch-buffer 'helm-buffer)
(defalias 'my-mini          'helm-recentf)
(defalias 'my-find-recentf  'helm-recentf)

(with-eval-after-load 'hydra
  (pretty-hydra-define hydra-launcher (:color teal :title "Overview")
    ("Groups"
     (("b"    hydra-buffers/body   "+ buffers")
      ("c"    hydra-comments/body  "+ comment")
      ("f"    hydra-files/body     "+ files")
      ("p"    hydra-projects/body  "+ project")
      ("T"    hydra-toggles/body   "+ toggles")
      ("w"    hydra-windows/body   "+ windows")
      ("m"    major-mode-hydra     "+ major-mode")
      ("l"    hydra-lsp/body       "+ lsp")
      ("e"    hydra-motions/body   "+ motions"))

     "Actions"
     (("Qq" save-buffers-kill-emacs	 "quit emacs"		:exit t)
      ("Qr" restart-emacs		 "restart emacs"	:exit t)
      ("!"  shell-command		 "run shell command")
      (":"  eval-expression		 "eval lisp expression")
      ("d"  dired			 "dired")
      ("D"  dired-other-window		 "dired(other window)")
      ("t"  treemacs                     "treemacs")
      ("E"  er/expand-region		 "expand region")
      ("g"  git-messenger:popup-message "git: popup commit"))

     "Others"
     (("z" font-scale/body "font scale"))))

  (pretty-hydra-define font-scale (:title "Font Scale Panel")
    (""
     (("+" (default-text-scale-increase) "zoom in")
      ("-" (default-text-scale-decrease) "zoom out")
      ("0" (default-text-scale-reset) "reset"))))

  (pretty-hydra-define hydra-motions (:color blue :title "Motions")
    ("Jump"
     (("l" avy-goto-line   "goto line")
      ("w" avy-goto-word-1 "goto word")
      ("c" avy-goto-char-2 "goto char"))
     "Expand"
     (("e" er/expand-region)      
      ("p" er/mark-inside-pairs)
      ("q" er/mark-inside-quotes))
     ))

  (pretty-hydra-define hydra-comments (:hint nil :color teal :exit t :title "Commentary Actions")
    (""
     (("b" comment-box)
      ("c" comment-dwim)
      ("l" comment-line)
      ("r" comment-region))))

  (pretty-hydra-define hydra-toggles
    (
     :pre (setq which-key-inhibit t)
     :post (setq which-key-inhibit nil)
     :title (with-faicon "toggle-on" "Toggles")
     :foreign-keys warn
     :quit-key "q"
     :exit t
     )
    ("Info/check/linting Modes"
     (("e" eldoc-mode "Echo Lisp objs" :toggle t)
      ("a" apheleia-mode "Code format" :toggle t)
      ("A" apheleia-global-mode "Format global" :toggle t)
      ("fc" flycheck-mode "Code linter" :toggle t)
      ("fs" flyspell-mode "Spell check" :toggle t)
      ("fp" flyspell-prog-mode "Spell check prog" :toggle t)
      ("fv" flycheck-verify-setup "Verify setup")
      ("ld" lsp-ui-doc-mode :toggle t)
      ("lp" lsp-ui-peek-mode :toggle t)
      ("ls" lsp-ui-sideline-mode :toggle t))
     "Edit/assistance"
     (("C-p" persp-mode-projectile-bridge-mode "Projectile bridge mode" :toggle t)
      ("C-j" ja-keys-minor-mode "My keys minor mode" :toggle t)
      ("C-A" global-auto-complete-mode "AC global" :toggle t)
      ("C-a" auto-complete-mode "AC local" :toggle t)
      ("C-l" electric-layout-mode "Elec layout" :toggle t)
      ("C-i" electric-indent-local-mode "Elec indent" :toggle t)
      ("C-q" electric-quote-local-mode "Elec quote" :toggle t)
      ("C-g" aggressive-indent-mode "Aggro indent" :toggle t)
      ("C-w" toggle-word-wrap "Word wrap" :toggle t)
      ("C-t" toggle-truncate-lines "Trunc lines" :toggle t)
      ("C-s" yas-minor-mode "Yas" :toggle t)
      ("C-c" whitespace-cleanup-mode "Whtspc cleanup" :toggle t)
      ("C-f" auto-fill-mode "Autofill" :toggle t) ; TODO: Toggle face does not change
      ("C-y" lispy-mode "Lispy" :toggle t))
     "Visual"
     (("e" jawa/toggle-org-emphasis-markers "Org emphasis" :toggle t)
      ("o" origami-mode "Origami" :toggle t)
      ("n" linum-mode "Linum" :toggle t)
      ("w" whitespace-mode "Whtspc" :toggle t)
      ("p" page-break-lines-mode "Page break lines" :toggle t)
      ("g" global-git-gutter-mode "Git gutter" :toggle t)
      ("i" fci-mode "Fill column ind" :toggle t)
      ("C-i" highlight-indent-guides-mode "Hilite indent" :toggle t)
      ("C-r" ivy-filthy-rich-mode "Ivy filty rich" :toggle t)
      ("ESC" nil "Quit"))))

  (pretty-hydra-define hydra-projects (:color blue :title "Projects")
    ("project actions"
     (("p" helm-projectile                  "project overview")
      ("b" helm-projectile-switch-to-buffer "project buffers")
      ("S" helm-projectile-switch-project   "switch project")
      ("s" helm-projectile-ag               "find in project")
      ("g" helm-projectile-grep             "grep in project")
      ("f" helm-projectile-find-file        "find file in project" :exit t)
      ("d" helm-projectile-find-dir         "find dir in project"  :exit t)
      ("i" projectile-invalidate-cache      "invalidate cache"     :color blue)
      )))

  (pretty-hydra-define hydra-buffers (:hint nil :color teal :title "Buffer Management Commands")
    ("Actions"
     (("b" helm-buffers-list "switch-buffer")
      ("d" kill-this-buffer  "kill current buffer")
      ("O" kill-other-buffers "kill other buffers")
      ("m" switch-to-modified-buffer)
      ("s" helm-swoop         "find in buffer")
      ("i" helm-imenu         "fuzzy-search-imenu")
      ("S" switch-to-scratch))))

  (pretty-hydra-define hydra-files (:hint nil :color teal :title "Files Commands")
    ("Find"
     (("f" find-file       "find file"   :exit t)
      ("e" open-init-el    "open init.el" :exit t)
      ("r" helm-recentf    "find recentf" :exit t)
      )))

  (pretty-hydra-define hydra-windows (:hint nil :title "Window Management")
    ("Switch"
     (("d" delete-window "delete window" :exit t)
      ("o" other-window "select other window" :exit t)
      ("O" delete-other-windows "delete other windiws" :exit t)
      ("w" ace-window "select window" :exit t)
      ("|" split-window-right "split window right" :exit t)
      ("-" split-window-below "split-window-below" :exit t))
     "Resize"
     (("+" enlarge-window "increase window")
      ("-" shrink-window "decrease window")
      ("max" maximize-window "maximize window")
      ("min" minimize-window "minimize window"))
     "Movement"
     (("h" windmove-left )
      ("j" windmove-down )
      ("k" windmove-up )
      ("l" windmove-right ))
     "Winner"
     (("u" winner-undo)
      ("U" winner-redo))
     ))

  (pretty-hydra-define hydra-lsp (:title "LSP Commands")
    ("Server"
     (("M-s" lsp-describe-session)
      ("M-r" lsp-restart-workspace)
      ("S" lsp-shutdown-workspace))
     "Buffer"
     (("f" lsp-format-buffer "format")
      ("m" lsp-ui-imenu "imenu")
      ("x" lsp-execute-code-action "execute action"))
     "Symbol"
     (("d" lsp-find-declaration "declaration"        :exit t)
      ("D" lsp-ui-peek-find-definitions "definition" :exit t)
      ("R" lsp-ui-peek-find-references "references"  :exit t)
      ("l" lsp-ivy-workspace-symbol "symbol")
      ("L" lsp-ivy-global-workspace-symbol "symbol(global)"))
     ""
     (("i" lsp-ui-peek-find-implementation "implementation")
      ("t" lsp-find-type-definition "type")
      ("s" lsp-signature-help "signature")
      ("o" lsp-describe-thing-at-point "documentation")
      ("r" lsp-rename "rename"))
     ))
  )
;; Global
(global-set-key (kbd "<f1>")	'helm-apropos)
;; (global-set-key (kbd "<f8>")	'my/treemacs-select-window)
(global-set-key (kbd "C-`")	'toggle-eshell-project-root)
(global-set-key (kbd "C-/")	'comment-line)
(global-set-key (kbd "C-M-l")	'indent-whole-buffer)
(global-set-key (kbd "C-j")	'ace-window)
(global-set-key (kbd "C-s")	'helm-swoop)
(global-set-key (kbd "C-x C-f")	'find-file)
(global-set-key (kbd "C-x b")	'my-mini)
(global-set-key (kbd "C-x c b")	'ivy-resume)
;; (global-set-key (kbd "M-x")	'M-x)
;; (global-set-key (kbd "≈")	'M-x)
(global-set-key (kbd "C-c q r") 'restart-emacs)
(global-set-key (kbd "C-=")	'er/expand-region)
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)
(global-set-key (kbd "C-c l")	'avy-goto-line)
(global-set-key (kbd "C-h m")   'describe-mode)
(global-set-key (kbd "C-,")     'hydra-launcher/body)
(global-set-key (kbd "C-<mouse-1>") 'evil-goto-definition)

;; Mirror Mode
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; Evil
(with-eval-after-load 'evil-maps
  ;; Normal state
  (define-key evil-normal-state-map "H"  'avy-goto-char-in-line)
  (define-key evil-normal-state-map "J"	 'avy-goto-word-1-below)
  (define-key evil-normal-state-map "K"	 'avy-goto-word-1-above)
  (define-key evil-normal-state-map "L"  'avy-goto-char-in-line)
  (define-key evil-normal-state-map "u"	 'undo-tree-undo)
  (define-key evil-normal-state-map "U"	 'undo-tree-redo)
  ;; (define-key evil-normal-state-map "gj" 'evil-join)
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-launcher/body)
  ;; (define-key evil-normal-state-map (kbd "\\") 'hydra-launcher/body)

  ;; Visual state
  (define-key evil-visual-state-map (kbd "SPC") 'hydra-launcher/body)
  ;; (define-key evil-visual-state-map (kbd "\\") 'hydra-launcher/body)

  ;; Insert state
  (define-key evil-insert-state-map "\C-e" 'move-end-of-line)
  (define-key evil-insert-state-map "\C-a" 'move-begining-of-line))

(provide 'init-keybindings)
;;; init-keybindings ends here
