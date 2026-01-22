;;; init-theme.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures themes and modeline for Emacs.
;; It includes theme classification, random theme loading, and theme persistence.

;;; Code:

;; Icons
(use-package all-the-icons)

;; Font settings for graphical displays
(when (display-graphic-p)
  (set-face-attribute 'default nil :font my-enfont)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset my-cnfont)))

;; Doom themes configuration
(use-package doom-themes
  :defer t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; Modeline configuration
(setq column-number-mode t)

(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar t)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-icon t)
  (doom-modeline-modal-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-number-limit 99)
  (doom-modeline-lsp t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info t)
  (doom-modeline-workspace-name t)
  (doom-modeline-env-go-executable "go")
  (doom-modeline-position-column-format '("C%c"))
  (doom-modeline-position-column-line-format '("L%lC%c"))
  :init
  (doom-modeline-mode 1))

;; Additional theme packages
(use-package kaolin-themes)

;; EF themes from GitHub
(use-package ef-themes
  :straight (:host github :repo "protesilaos/ef-themes" :files ("*.el")))

(use-package catppuccin-theme
  :init
  (setq catppuccin-flavor 'latte))

(use-package nano-theme
  :straight (:host github :repo "rougier/nano-theme"))

;;; Theme classification and utilities

(defvar classified-themes
  '(:dark (doom-zenburn doom-xcode doom-wilmersdorf doom-vibrant
          doom-tomorrow-night doom-tomorrow-day doom-tokyo-night
          doom-spacegrey doom-sourcerer doom-solarized-light
          doom-solarized-dark doom-solarized-dark-high-contrast
          doom-snazzy doom-shades-of-purple doom-rouge doom-plain
          doom-plain-dark doom-pine doom-peacock doom-palenight
          doom-outrun-electric doom-opera doom-one doom-old-hope
          doom-oksolar-dark doom-oceanic-next doom-nova doom-nord
          doom-nord-aurora doom-moonlight doom-monokai-spectrum
          doom-monokai-ristretto doom-monokai-pro doom-monokai-octagon
          doom-monokai-machine doom-monokai-classic doom-molokai
          doom-miramare doom-meltbus doom-material doom-material-dark
          doom-manegarm doom-laserwave doom-lantern doom-ir-black
          doom-horizon doom-homage-black doom-henna doom-gruvbox
          doom-feather-dark doom-fairy-floss doom-ephemeral doom-dracula
          doom-dark+ doom-city-lights doom-challenger-deep
          doom-bluloco-dark doom-badger doom-ayu-mirage doom-ayu-dark
          doom-acario-dark doom-Iosvkem doom-1337 kaolin-valley-dark
          kaolin-temple kaolin-shiva kaolin-ocean kaolin-mono-dark
          kaolin-galaxy kaolin-eclipse kaolin-dark kaolin-bubblegum
          kaolin-blossom kaolin-aurora ef-winter ef-tritanopia-dark
          ef-trio-dark ef-symbiosis ef-rosa ef-night ef-melissa-dark
          ef-maris-dark ef-elea-dark ef-duo-dark ef-dream
          ef-deuteranopia-dark ef-dark ef-cherie ef-bio ef-autumn
          catppuccin wombat wheatgrass tsdh-dark tango-dark
          modus-vivendi misterioso manoj-dark leuven-dark deeper-blue)
    :light (doom-opera-light doom-one-light doom-oksolar-light
            doom-nord-light doom-homage-white doom-gruvbox-light
            doom-flatwhite doom-feather-light doom-earl-grey
            doom-bluloco-light doom-ayu-light doom-acario-light
            kaolin-valley-light kaolin-mono-light kaolin-light
            kaolin-breeze ef-tritanopia-light ef-trio-light ef-summer
            ef-spring ef-reverie ef-melissa-light ef-maris-light
            ef-light ef-kassio ef-frost ef-elea-light ef-duo-light
            ef-deuteranopia-light ef-day ef-cyprus ef-arbutus
            whiteboard tsdh-light tango modus-operandi))
  "Plist of classified themes: :dark and :light.")

(defun classify-new-themes (dark-themes light-themes)
  "Classify themes not already in DARK-THEMES or LIGHT-THEMES using `background-mode`."
  (let ((all-themes (custom-available-themes))
        (new-dark-themes dark-themes)
        (new-light-themes light-themes))
    ;; Filter out unclassified themes
    (dolist (theme all-themes)
      (unless (or (member theme dark-themes)
                  (member theme light-themes))
        (load-theme theme t t) ; Load theme but don't enable
        (let ((mode (frame-parameter nil 'background-mode)))
          (cond
           ((eq mode 'dark) (push theme new-dark-themes))
           ((eq mode 'light) (push theme new-light-themes))))))
    ;; Return updated classification
    (list :dark new-dark-themes :light new-light-themes)))

;; To update classified-themes, run:
;; (message "%s" (classify-new-themes (plist-get classified-themes :dark)
;;                                    (plist-get classified-themes :light)))

(defun load-random-light-theme ()
  "Randomly select and apply a light theme."
  (interactive)
  (let ((theme (random-choice (plist-get classified-themes :light))))
    (load-theme theme t)))

(defun load-random-dark-theme ()
  "Randomly select and apply a dark theme."
  (interactive)
  (let ((theme (random-choice (plist-get classified-themes :dark))))
    (load-theme theme t)))

(defun consult-light-theme ()
  "Prompt user to select a light theme from `classified-themes` and apply it."
  (interactive)
  (let* ((light-themes (plist-get classified-themes :light))
         (theme (completing-read "Select a light theme: " light-themes)))
    (when (and theme (intern theme))
      (load-theme (intern theme) t))))

(defun consult-dark-theme ()
  "Prompt user to select a dark theme from `classified-themes` and apply it."
  (interactive)
  (let* ((dark-themes (plist-get classified-themes :dark))
         (theme (completing-read "Select a dark theme: " dark-themes)))
    (when (and theme (intern theme))
      (load-theme (intern theme) t))))

;;; Theme persistence
(defvar chosen-theme nil
  "The currently chosen theme, saved to custom-file.")

(defun save-chosen-theme (theme &rest _args)
  "Save THEME to `chosen-theme` in custom-file."
  (customize-save-variable 'chosen-theme theme)
  (message "Theme saved: %s" theme))

(advice-add 'load-theme :after #'save-chosen-theme)

;; Load initial theme based on conditions
(cond
 (chosen-theme (load-theme chosen-theme :no-confirm))
 (*gui*        (load-theme my-gui-theme :no-confirm))
 (t            (load-theme my-tui-theme :no-confirm)))

(provide 'init-theme)

;;; init-theme.el ends here
