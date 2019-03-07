(defvar en-fonts '("Source Code Pro" 13 "Courier New" 13))
(defvar cn-fonts '("宋体" 15 "微软雅黑" 15))

(defun core-font/exists-p (font-name)
  "检查字体是否存在."
  (if (null (x-list-fonts font-name)) nil t))

(defun core-font/use-en (font-name font-size)
  "设置英文字体."
  (set-face-attribute 'default nil
		      :font (format "%s:pixelsize=%d" font-name font-size)
		      :weight 'normal))

(defun core-font/use-cn (font-name font-size)
  "设置中文字体."
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
		      (font-spec :family font-name :size font-size))))

(defun core-font/use-list (font-list font-func)
  "设置字体列表，从列表头开始查找存在的字体并使用该字体."
  (unless (null font-list)
    (let ((font-name (car font-list))
	  (font-size (cadr font-list)))
      (if (core-font/exists-p font-name)
	  (funcall font-func font-name font-size)
	(core-font/use-list (cddr font-list) font-func)))))

(defun core-font/use-en-list (font-list)
  (core-font/use-list font-list 'core-font/use-en))

(defun core-font/use-cn-list (font-list)
  (core-font/use-list font-list 'core-font/use-cn))


(defun core-font/init ()
  (when (display-graphic-p)
    (core-font/use-en-list en-fonts)
    (core-font/use-cn-list cn-fonts)))

(core-font/init)

(provide 'init-font)
