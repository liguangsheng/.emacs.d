;;; init-python.el --- Python mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Python mode with formatting support.

;;; Code:

(use-package python-mode
  :config
  ;; Use format-all-buffer instead of my-format-buffer for formatting
  (define-key python-mode-map [remap my-format-buffer] #'format-all-buffer))

(provide 'init-python)

;;; init-python.el ends here
