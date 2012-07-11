;;; dim-init-debian.el --- Dimitri Fontaine

(setq color-theme-libraries nil)

(when (eq window-system 'x)
  ;; switched from tango to solarized --- http://ethanschoonover.com/solarized
  ;;
  (progn
    (setq color-theme-libraries nil)
    (require 'color-theme)
    (require 'color-theme-tango)
    (color-theme-tango)
    (set-face-attribute 'hl-line nil :background "chocolate4"))

  ;; adapt shell colors to tango theme
  (progn
    (require 'ansi-color)
    (setq ansi-color-names-vector
	  (vector (frame-parameter nil 'background-color)
		  "#f57900" "#8ae234" "#edd400" "#729fcf"
		  "#ad7fa8" "cyan3" "#eeeeec")
	  ansi-term-color-vector ansi-color-names-vector
	  ansi-color-map (ansi-color-make-color-map)))

  ;; (require 'naquadah-theme)

  ;; (progn
  ;;   (el-get 'sync 'color-theme-solarized)
  ;;   (color-theme-solarized 'dark)
  ;;   (color-theme-solarized 'light)

  ;;   this theme sets the hl-line and the transient-mark-mode background
  ;;   faces the same, which is not cool.
  ;;   (set-face-attribute 'hl-line nil :background "grey90"))

  ;; be easy on the eyes
  ;; (if (equal (get-screen-dimensions) '(1440 900))
  ;;     (set-face-font 'default "Monospace-10")
  ;;   (set-face-font 'default "Monospace-12"))

  (set-face-font 'default "Monospace-10"))

(require 'dim-desktop)
(provide 'dim-init-debian)
