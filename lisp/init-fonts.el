;;; init-fonts.el 

;;; Commentary:
;;
;; Recommend Fonts:
;; 
;; Latin: https://fonts.google.com/specimen/Roboto+Mono
;; Latin: https://github.com/tonsky/FiraCode
;; Latin: https://github.com/adobe-fonts/source-code-pro
;; Latin: https://github.com/aosp-mirror/platform_frameworks_base/tree/master/data/fonts;; Code:
;; Chinese: http://wenq.org/wqy2/index.cgi

(defvar prefer-cn-font nil)

(defvar prefer-en-font nil)

(defconst cjk-charsets '(kana han symbol cjk-misc bopomofo))

(defconst fallback-unicode-fonts '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol"))

(defconst fallback-cn-fonts `(,(font-spec :family "WenQuanYi Micro Hei" :size 14)
			      ,(font-spec :family "WenQuanYi Zen Hei" :size 14)
			      ,(font-spec :family "Microsoft Yahei" :size 14)))

(defconst fallback-fonts '("Fira Code:pixelsize=12"
			   "Source Code Pro:pixelsize=12"
			   "Droid Sans Mono:pixelsize=12"
			   "Menlo:size=13"
			   "Monoco:size=13"
			   "Consolas:size=13"
			   "Courier New:size=13"
			   "monospace:size=13"))

(defun must-list (v)
  (if (listp v) v (list v)))

(defun font-installed-p (font)
  "Check if font is available."
  (cond ((stringp font) (find-font (font-spec :name font)))
	((fontp font) (find-font font))
	(t nil)))

(defun setup-unicode-font (&optional font)
  (cl-loop for font-item in (append (must-list font) fallback-unicode-fonts)
	   when (font-installed-p font-item)
           return (set-fontset-font t 'unicode font-item nil 'prepend)))


(defun setup-cnfont (&optional font)
  (cl-loop for font-item in (append (must-list font) fallback-cn-fonts)
	   when (font-installed-p font-item)
	   do (cl-loop for charset in cjk-charsets
		       do (set-fontset-font t charset font-item))
	   return t))

(defun setup-font (&optional font)
  (cl-loop for font-item in (append `(,font) fallback-fonts)
	   ;; when (font-installed-p font-item)
	   do (progn
		(set-frame-font font-item)
		)
	   return font-item))

(defun setup-fonts ()
  (interactive)
  (progn
    (setup-unicode-font)
    (setup-font   preferences/font)
    (setup-cnfont preferences/cnfont)
    ))

(when *gui* (setup-fonts))
;; (when *gui* (add-hook 'after-init-hook #'setup-fonts))


;; Font Example:
;; | 你好  | 世界  |
;; | Hello | World |
;; 千山鸟飞绝，万径人踪灭。
;; 孤舟蓑笠翁，独钓寒江雪。
;; abcdefghijklmnopqrstuvwxyz0123456789
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
;; :smile::cry::bear::bike::rose::thumbup::thumbdown::ok:
;; αβγδεζηθικlμνξοpρsτυφχψω
;; ΑΒΓΔΕΖΗΘΙΚλΜΝΞΟπΡσΤΥΦΧΨΩ

(provide 'init-fonts)
;;; init-fonts.el ends here
