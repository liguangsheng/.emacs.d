;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme paper "A light color theme for Emacs")

(custom-theme-set-variables
 'paper
 '(linum-format " %7i "))

(let ((*background*         "#EEE")
      (*comments*           "#bcbcbc")
      (*constant*           "#d75f00")
      (*current-line*       "#e4e4e4")
      (*cursor-bg*          "#005f87")
      (*cursor-fg*          "#eee")
      (*keywords*           "#005faf")

      ;; Sidebar line numbers
      (*line-number*        "#EEE")
      (*line-fg*            "#b2b2b2")

      (*type-face*          "#d70087")
      (*method-declaration* "#008700")
      (*mode-line-bg*       "#005f87")
      (*mode-inactive-bg*   "#d0d0d0")
      (*mode-line-fg*       "#e4e4e4")
      (*mode-inactive-fg*   "#444")
      (*normal*             "#444")
      (*number*             "#d75f00")
      (*operators*          "#FF80F4")
      (*warning*            "#FF6C60")
      (*regexp*             "#A63A62")
      (*string*             "#5f8700")
      (*variable*           "#FD971F")
      (*visual-selection*   "#878787"))

  (custom-theme-set-faces
   'paper

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background* :foreground, *normal*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line*))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection*))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground, *constant*))))
   `(font-lock-doc-face ((t (:foreground, *string*))))
   `(font-lock-doc-string-face ((t (:foreground, *string*))))
   `(font-lock-function-name-face ((t (:foreground, *method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground, *keywords*))))
   `(font-lock-negation-char-face ((t (:foreground, *warning*))))
   `(font-lock-number-face ((t (:foreground, *number*))))
   `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *type-face*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *warning*))))

   ;; GUI
   `(fringe ((t (:background, *background*))))
   `(linum ((t (:background, *line-number* :foreground, *line-fg*))))
   `(minibuffer-prompt ((t (:foreground, *variable*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-inactive-bg* :foreground, *mode-inactive-fg*))))
   `(cursor ((t (:background, *cursor-bg* :foreground, *cursor-fg*))))
   `(text-cursor ((t (:background, *cursor-bg*))))
   `(vertical-border ((t (:background, *background*)))) ;; between splits


   ;; show-paren
   `(show-paren-mismatch ((t (:background, *warning* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *keywords* :foreground, *normal* :weight bold))))

   ;; search
   `(isearch ((t (:background, *regexp* :foreground, *visual-selection*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *visual-selection*))))
   )

  ;; emacs >= 26.1
  (when (>= emacs-major-version 26)
    (custom-theme-set-faces
     'paper
     `(line-number ((t (:foreground, *line-fg* :background, *background*))))
     `(line-number-current-line ((t (:background, *current-line* :weight bold))))))
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'paper)

;; Local Variables:
;; no-byte-compile: t
;; End:

