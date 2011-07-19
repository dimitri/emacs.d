;;; init.el --- Dimitri Fontaine
;;
;; load-path has to be explicitly setup first

(defun dim:add-my-extra-load-paths (&optional paths)
  "define a list of paths to add to load-path and add each of them"
  (let ((dim:paths '("~/dev/emacs/el-get"
		     "~/dev/emacs.d"
		     "~/dev/emacs.d/rcirc"
		     "~/dev/emacs.d/lib"
		     "~/dev/emacs/rcirc-groups"
		     )))
    (dolist (path (or paths dim:paths))
      (setq load-path (cons path load-path)))))

;; extension du load-path proprement dite.
(dim:add-my-extra-load-paths)

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; custom loading
(setq custom-file "~/dev/emacs.d/dim-custom.el")
(load custom-file)

;; first the common stuff
(require 'dim-lib)
(require 'dim-ports)
(require 'dim-visual-common)

;; now the external packages we depend upon
(require 'dim-packages)

(when-running-macosx (require 'dim-init-macosx))
(when-running-debian-or-ubuntu (require 'dim-init-debian))

;; that's tapoueh.org git depo
(load "~/dev/tapoueh.org/tapoueh.el")

;; that's local projects, and some other local muse sites
(require 'dim-projects)

;; configuration rcirc, and global shortcut to connect to servers
(require 'dim-rcirc)

;; bindings --- depends on rcirc being loaded
(require 'dim-escreen)
(require 'dim-keys)
(require 'dim-modes)
(require 'dim-tels-pro)

;; empower M-term to remote hosts
(require 'cssh)

;; local utilities
(when (and (get-domain-name)
	   (string-match "hi-media" (get-domain-name)))
  (require 'dim-check-prefix)
  (require 'dim-hi-media-vpn))

;; and the session
(setq desktop-restore-eager 20
      desktop-lazy-verbose nil)
(desktop-save-mode 1)
(savehist-mode 1)
