;;; init-icons.el -- icons configuration

;;; Commentary:

;;; Code:

(defvar perferences/enable-icons *gui*)

(use-package all-the-icons :if perferences/enable-icons)

(use-package all-the-icons-ivy
  :after all-the-icons ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :after all-the-icons ivy
  :init (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'init-icons)
;;; init-icons.el ends here
