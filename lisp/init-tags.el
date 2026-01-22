;;; init-tags.el --- Ctags and tags navigation -*- lexical-binding: t; ---

;;; Commentary:
;; This file configures Citre for enhanced tags-based code navigation
;; and completion using ctags and GNU Global.

;;; Code:

(use-package citre
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)

  ;; Bind frequently used commands
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

  (my-leader-def
    "c c" '(:ignore t :wk "citre")
    "c c d" '(citre-jump      :wk "Jump by tags")
    "c c J" '(citre-jump-back :wk "Jump back")
    "c c p" 'citre-ace-peek
    "c c u" 'citre-update-this-tags-file)

  (setq
   ;; Set these if readtags/ctags is not in your path.
   citre-readtags-program (executable-find "/usr/bin/readtags")
   citre-ctags-program (executable-find "/usr/bin/ctags")
   citre-gtags-program "/usr/bin/gtags"
   citre-global-program "/usr/bin/global"
   citre-gtags-args '("--compact")
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(graphql-mode
                                         protobuf-mode)
   citre-enable-capf-integration nil)

  (defun citre-mix-multi-backends ()
    "Mix multiple completion backends for citre."
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-buster
                  (cape-capf-super
                   ;; #'tabnine-completion-at-point
                   #'citre-completion-at-point
                   #'cape-keyword
                   #'cape-abbrev
                   #'cape-file
                   #'cape-dabbrev)
                  'equal)))

    (define-key citre-mode-map [remap xref-find-definitions] #'citre-jump))

  (add-hook 'citre-mode-hook #'citre-mix-multi-backends))

(provide 'init-tags)

;;; init-tags.el ends here
