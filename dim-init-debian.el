;;; dim-init-debian.el --- Dimitri Fontaine

(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)

;; be easy on the eyes
(if (equal (get-screen-dimensions) '(1440 900))
    (set-face-font 'default "Monospace-10")
  (set-face-font 'default "Monospace-12"))

;; apt-get install squid
;(setenv "http_proxy" "127.0.0.1:3128")

(provide 'dim-init-debian)
