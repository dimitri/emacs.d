;;; dim-init-macosx.el --- Dimitri Fontaine
;;
;; Sous mac plein de softs intéressants sont dans /sw/bin

(setenv "PATH" (concat (getenv "PATH") ":" "/sw/bin" ":" "/usr/local/git/bin"))
(add-to-list 'exec-path "/sw/bin")
(add-to-list 'exec-path "/usr/local/git/bin")

(require 'info)
(add-to-list 'Info-directory-list (concat invocation-directory "../Resources/info"))
(add-to-list 'Info-directory-list "/sw/share/info/")

;; special visual tweaking
(setq color-theme-libraries nil)
(load "~/.emacs.d/el-get/color-theme/color-theme.el")
(require 'color-theme)
(require 'color-theme-emacs23-default)
(color-theme-emacs23-default)

;; here hiding the menu bar makes no sense
(menu-bar-mode 1)

(require 'woman)
(add-to-list 'woman-manpath "/sw/share/man")

;; chuuut
(setq ring-bell-function 'ignore)

;; MacOSX specific setting
(setq mac-allow-anti-aliasing t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
;(ns-set-background-alpha 0.9)
(add-to-list 'default-frame-alist '(alpha . 90))

;; font, taille et position
(set-face-font 'default "Monaco-13")
(set-frame-position (selected-frame) 60 30)

(if (equal (get-screen-dimensions) '(2560 1440))
    (progn
      (set-frame-size (selected-frame) 174 81)
      (let ((right-frame (make-frame '((top . 30)))))
	(set-frame-parameter right-frame 'font "Monaco-13")
	(set-frame-parameter right-frame 'left 1480)
	(set-frame-size (selected-frame) 130 81)))
  (set-frame-size (selected-frame) 168 49))

;; some special for offlineimap
(require 'dim-offlineimap)
(setq dim:offlineimap-bin "/sw/bin/offlineimap"
      dim:offlineimap-options "")

;; home only usage
(load "fun/betaseries.el")

(provide 'dim-init-macosx)
