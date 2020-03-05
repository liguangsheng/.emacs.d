;;; init-smooth-scrolling.el --- Smooth scrolling configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Feature: smooth-scrolling
(defvar smooth-scrolling? t)

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 1
	smooth-scroll-strict-margins t)
  :config
  (when smooth-scrolling?
    (smooth-scrolling-mode 1)))

(provide 'init-smooth-scrolling)
;;; init-smooth-scrolling.el ends here
