;;; init-fonts.el -- 字体相关配置

;;; Commentary:

;;; Code:

;;; Fonts:
;; recommend: https://github.com/powerline/fonts
;; Font Example:
;; 春眠不觉晓，处处闻啼鸟
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; 0123456789

(defvar prefer-cn-font nil)
(defvar prefer-en-font nil)

(setq-default cjk-charsets  '(kana han symbol cjk-misc bopomofo)
	      default-unicode-fonts '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
	      default-cn-fonts      `(,(font-spec :family "WenQuanYi Micro Hei" :height 90)
				      ,(font-spec :family "Microsoft Yahei" :height 90))
	      default-en-fonts      '("Droid Sans Mono:size=13"
				      "Menlo:size=13"
				      "Monoco:size=13"
				      "Consolas:size=13"
				      "Courier New:size=13"
				      "monospace:size=13"))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  (interactive)
  ;; set unicode fonts
  (cl-loop for font in default-unicode-fonts
	   when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; set en fonts
  (let ((fonts (copy-sequence default-en-fonts)))
    (when (or (stringp prefer-en-font) (fontp prefer-en-font)) (push prefer-en-font fonts))
    (cl-loop for font in fonts
	     when (font-installed-p font)
	     return (set-frame-font font)))

  ;; set cjk fonts
  (let ((fonts (copy-sequence default-cn-fonts)))
    (when (or (stringp prefer-cn-font) (fontp prefer-cn-font)) (push prefer-cn-font fonts))
    (cl-loop for font in fonts
	     do (cl-loop for charset in cjk-charsets
			 do (set-fontset-font t charset font))))
  )

(when (display-graphic-p)
  (add-hook 'after-init-hook 'setup-fonts)
  (add-hook 'minibuffer-setup-hook '(lambda () (set (make-local-variable 'face-remapping-alist) '((default :height 90))))))

(provide 'init-fonts)
;;; init-fonts.el ends here
