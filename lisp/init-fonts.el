;;; init-fonts.el

;;; Commentary:

(defconst cjk-charsets '(kana han symbol cjk-misc bopomofo))
(defconst fallback-unicode-fonts '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol"))

(defconst fallback-cn-fonts `(
							  "LXGW WenKai Mono:pixelsize=14"
							  "WenQuanYi Micro Hei:pixelsize=14"
							  "AR PL UKai CN:pixelsize=14"
							  "WenQuanYi Zen Hei:pixelsize=14"
							  "Microsoft Yahei:pixelsize=14"
							  ))

(defconst fallback-fonts '(
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
						   "monospace:size=13"
						   ))

(defun font-installed-p (font)
  "Check if font is available."
  (cond ((stringp font) (find-font (font-spec :name font)))
		((fontp font) (find-font font))
		(t nil)))

(defun setup-unicode-font (&optional font)
  (cl-loop for font-item in (append (ensure-list font) fallback-unicode-fonts)
		   when (font-installed-p font-item)
           return (set-fontset-font t 'unicode font-item nil 'prepend)))


(defun setup-cnfont (&optional font)
  (cl-loop for font-item in (append (ensure-list font) fallback-cn-fonts)
		   when (font-installed-p font-item)
		   do (cl-loop for charset in cjk-charsets
					   do (set-fontset-font t charset font-item))
		   return t))

(defun setup-enfont (&optional font)
  (cl-loop for font-item in (append `(,font) fallback-fonts)
		   ;; when (font-installed-p font-item)
		   do (progn
				(set-frame-font font-item)
				(add-to-list 'default-frame-alist `(font . ,font-item))
				)
		   return font-item))


(defun setup-fonts ()
  (interactive)
  (progn
    (setup-unicode-font)
    (setup-enfont my-enfont)
    (setup-cnfont my-cnfont)
    ))

(defun increase-font-size ()
  "Increase font size in the current buffer."
  (interactive)
  (let ((current-size (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ current-size 10))))

(defun decrease-font-size ()
  "Decrease font size in the current buffer."
  (interactive)
  (let ((current-size (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- current-size 10))))

(defun reset-font-size ()
  "Reset font size in the current buffer to default."
  (interactive)
  (set-face-attribute 'default nil :height default-frame-font-height))

(defvar default-frame-font-height
  (face-attribute 'default :height)
  "Default font height for the frame.")

(global-set-key (kbd "C-M-=") 'increase-font-size)
(global-set-key (kbd "C-M--") 'decrease-font-size)
(global-set-key (kbd "C-M-0") 'reset-font-size)

(when (display-graphic-p) (setup-fonts))

;; Font Example:
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
