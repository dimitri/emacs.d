;;; dim-init-macosx.el --- Dimitri Fontaine
;;
;; Sous mac plein de softs intéressants sont dans /sw/bin

(setenv "PATH" (concat (getenv "PATH") ":" "/sw/bin" ":" "/usr/local/git/bin"))
(add-to-list 'exec-path "/sw/bin")
(add-to-list 'exec-path "/usr/local/git/bin")

;; don't forget platform specific things
(dim:add-my-extra-load-paths '("~/.emacs.d/nxhtml"
			       "~/.emacs.d/bbdb-2.35/lisp"
			       "~/.emacs.d/bbdb-2.35/bits"
			       "~/.emacs.d/w3m/emacs-w3m/"
			       "~/.emacs.d/color-theme-6.6.0"
			       "~/.emacs.d/color-theme-6.6.0/themes"))

;; special visual tweaking
(require 'color-theme)
(require 'color-theme-emacs23-default)
(color-theme-emacs23-default)

;; here hiding the menu bar makes no sens
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
      (set-frame-size (selected-frame) 174 90)
      (let ((right-frame (make-frame '((top . 30)))))
	(set-frame-parameter right-frame 'font "Monaco-13")
	(set-frame-parameter right-frame 'left 1480)
	(set-frame-size (selected-frame) 130 90)))
  (set-frame-size (selected-frame) 192 55))

;;
;; cssh special settings for when working from home
;;
(require 'cssh)
(setq cssh-remote-user "dfontaine")
(setq cssh-override-nameserver "ns1.hi-media-techno.com")
(setq cssh-override-domain ".hi-media-techno.com")
(setq cssh-hostname-resolve 'cssh-override-resolver)
(setq cssh-hostname-resolve 'cssh-default-resolver)

;; some special for magit
(require 'magit)
(setq magit-git-executable "/usr/local/git/bin/git")

;; some special for offlineimap
(require 'dim-offlineimap)
(setq dim:offlineimap-bin "/sw/bin/offlineimap"
      dim:offlineimap-options "")

(provide 'dim-init-macosx)
