(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :init
  ;; Add Emacs-Lisp for tree-sitter:
  ;;
  ;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
  ;; 2. gcc ./src/parser.c -fPIC -I./ --shared -o elisp.so
  ;; 3. cp ./elisp.so ~/.tree-sitter-langs/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
  ;; (tree-sitter-load 'elisp)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))
  )

(provide 'init-tree-sitter)
