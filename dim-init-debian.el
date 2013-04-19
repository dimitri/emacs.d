;;; dim-init-debian.el --- Dimitri Fontaine

(setq color-theme-libraries nil)

(when (eq window-system 'x)
  ;; switched from tango to solarized --- http://ethanschoonover.com/solarized
  ;;
  (progn
    (setq color-theme-libraries nil)
    (require 'color-theme)
    (require 'color-theme-tango-2)
    (color-theme-tango-2)
    ;; (set-face-attribute 'hl-line nil :background "chocolate4")
    (set-face-attribute 'hl-line nil :background "grey20"))

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

  ;;   ;; this theme sets the hl-line and the transient-mark-mode background
  ;;   ;; faces the same, which is not cool.
  ;;   (set-face-attribute 'hl-line nil :background "grey90"))

  ;; be easy on the eyes
  ;; (if (equal (get-screen-dimensions) '(1440 900))
  ;;     (set-face-font 'default "Monospace-10")
  ;;   (set-face-font 'default "Monospace-12"))

  ;; (set-face-font 'default "Inconsolata-12")
  (set-face-font 'default "Monospace-11")

  ;; adjust for when running in the squeeze VirtualBox
  (let ((screen-size (get-screen-dimensions)))
    ;; will need to add right frame size for 1400x900 too
    (if (or (equal screen-size '(2560 1440))	; when on the HOST
	    (equal screen-size '(2496 1418))) ; what VB X11 returns
	(set-frame-size (selected-frame) 170 60))
    (progn
      ;; 1400x900 display size, reported as (1384 878) by VB
      (set-frame-position (selected-frame) 10 8)
      (set-frame-size (selected-frame) 149 46))))

(require 'dim-desktop)
(provide 'dim-init-debian)
