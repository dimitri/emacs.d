;;; dim-init-debian.el --- Dimitri Fontaine

(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)

;; be easy on the eyes
(if (equal (get-screen-dimensions) '(1440 900))
    (set-face-font 'default "Monospace-10")
  (set-face-font 'default "Monospace-12"))

(provide 'dim-init-debian)
