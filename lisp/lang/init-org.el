(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ox-reveal)
(use-package org-re-reveal)

(provide 'init-org)
