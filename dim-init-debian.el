;;; dim-init-debian.el --- Dimitri Fontaine

(setq color-theme-libraries nil)

(when (eq window-system 'x)
  ;; switched from tango to solarized --- http://ethanschoonover.com/solarized
  ;;
  ;; (setq color-theme-libraries nil)
  ;; (require 'color-theme)
  ;; (require 'color-theme-tango)
  ;; (color-theme-tango)

  ;; (require 'naquadah-theme)

  ;; (el-get 'sync 'color-theme-solarized)
  ;; (color-theme-solarized 'dark)
  (color-theme-solarized 'light)

  ;; be easy on the eyes
  ;; (if (equal (get-screen-dimensions) '(1440 900))
  ;;     (set-face-font 'default "Monospace-10")
  ;;   (set-face-font 'default "Monospace-12"))

  (set-face-font 'default "Monospace-10"))

(require 'dim-desktop)
(provide 'dim-init-debian)
