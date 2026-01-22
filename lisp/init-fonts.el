;;; init-fonts.el --- Font configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures fonts for Emacs, including fallback fonts for
;; English, Chinese, and Unicode characters, along with font size controls.

;;; Code:

;; Font constants for different character sets
(defconst cjk-charsets '(kana han symbol cjk-misc bopomofo)
  "List of CJK character sets.")

(defconst fallback-unicode-fonts '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
  "Fallback fonts for Unicode symbols.")

(defconst fallback-cn-fonts `("LXGW WenKai Mono:pixelsize=14"
                              "WenQuanYi Micro Hei:pixelsize=14"
                              "AR PL UKai CN:pixelsize=14"
                              "WenQuanYi Zen Hei:pixelsize=14"
                              "Microsoft Yahei:pixelsize=14")
  "Fallback fonts for Chinese characters.")

(defconst fallback-fonts '("MonaspiceNeNerdFont:pixelsize=12:weight=Medium"
                           "JetBrainsMono Nerd Font:pixelsize=12:weight=medium"
                           "RobotoMono Nerd Font:pixelsize=12:weight=medium"
                           "Fira Code Nerd Font:pixelsize=12:weight=medium"
                           "Hack Nerd Font:pixelsize=12:weight=medium"
                           "DroidSansMono Nerd Font:pixelsize=12:weight=medium"
                           "Source Code Pro:pixelsize=12:weight=medium"
                           "Droid Sans Mono:pixelsize=12:weight=medium"
                           "Menlo:size=13"
                           "Monoco:size=13"
                           "Consolas:size=13"
                           "Courier New:size=13"
                           "monospace:size=13")
  "Fallback fonts for English text.")

;; Utility functions
(defun font-installed-p (font)
  "Check if FONT is available."
  (cond ((stringp font) (find-font (font-spec :name font)))
        ((fontp font) (find-font font))
        (t nil)))

(defun setup-unicode-font (&optional font)
  "Setup Unicode font with fallback fonts.
Optional FONT specifies a preferred font."
  (cl-loop for font-item in (append (ensure-list font) fallback-unicode-fonts)
           when (font-installed-p font-item)
           return (set-fontset-font t 'unicode font-item nil 'prepend)))

(defun setup-cnfont (&optional font)
  "Setup Chinese font for CJK charsets with fallback fonts.
Optional FONT specifies a preferred font."
  (cl-loop for font-item in (append (ensure-list font) fallback-cn-fonts)
           when (font-installed-p font-item)
           do (cl-loop for charset in cjk-charsets
                       do (set-fontset-font t charset font-item))
           return t))

(defun setup-enfont (&optional font)
  "Setup English font with fallback fonts.
Optional FONT specifies a preferred font."
  (cl-loop for font-item in (append `(,font) fallback-fonts)
           ;; when (font-installed-p font-item)
           do (progn
                (set-frame-font font-item)
                (add-to-list 'default-frame-alist `(font . ,font-item)))
           return font-item))

;; Main font setup function
(defun setup-fonts ()
  "Setup all fonts: Unicode, English, and Chinese."
  (interactive)
  (setup-unicode-font)
  (setup-enfont my-enfont)
  (setup-cnfont my-cnfont))

;; Font size adjustment functions
(defun increase-font-size ()
  "Increase font size by 10 points."
  (interactive)
  (let ((current-size (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ current-size 10))))

(defun decrease-font-size ()
  "Decrease font size by 10 points."
  (interactive)
  (let ((current-size (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- current-size 10))))

(defun reset-font-size ()
  "Reset font size to default."
  (interactive)
  (set-face-attribute 'default nil :height default-frame-font-height))

(defvar default-frame-font-height
  (face-attribute 'default :height)
  "Default font height for the frame.")

;; Key bindings for font size control
(global-set-key (kbd "C-M-=") 'increase-font-size)
(global-set-key (kbd "C-M--") 'decrease-font-size)
(global-set-key (kbd "C-M-0") 'reset-font-size)

;; Initialize fonts on graphical display
(when (display-graphic-p) (setup-fonts))

;; Font rendering examples (for testing):
;; | 你好  | 世界  |
;; | Hello | World |
;; 千山鸟飞绝，万径人踪灭。
;; 孤舟蓑笠翁，独钓寒江雪。
;; 🎉🎊🎁👍😊🌟🌈🍕🍔🍟🍺🍷🎂🍰🍩🍦🍭🎮🎬🎤🎧🎼🎸🎲🎳⚽
;; abcdefghijklmnopqrstuvwxyz0123456789
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
;; :smile::cry::bear::bike::rose::thumbup::thumbdown::ok:
;; αβγδεζηθικlμνξοpρsτυφχψω
;; ΑΒΓΔΕΖΗΘΙΚλΜΝΞΟπΡσΤΥΦΧΨΩ

(provide 'init-fonts)

;;; init-fonts.el ends here
