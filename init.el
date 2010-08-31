;;; init.el --- Dimitri Fontaine
;;
;; load-path has to be explicitly setup first

(defun dim:add-my-extra-load-paths (&optional paths)
  "define a list of paths to add to load-path and add each of them"
  (let ((dim:paths '("~/dev/emacs/el-get"
		     "~/dev/emacs.d"
		     "~/dev/emacs.d/rcirc"
		     "~/dev/emacs.d/lib"
		     "~/dev/emacs/cssh"
		     "~/dev/emacs/rcirc-groups"
		     "~/dev/tapoueh.org"
		     "~/.emacs.d/w3m/emacs-w3m/"
		     ;;"~/.emacs.d/emms/lisp/"
		     ;;"~/.emacs.d/magit"
		     )))
    (dolist (path (or paths dim:paths))
      (setq load-path (cons path load-path)))))

;; extension du load-path proprement dite.
(dim:add-my-extra-load-paths)

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; first the common stuff
(require 'dim-lib)
(require 'dim-ports)
(require 'dim-visual-common)

;; now the external packages we depend upon
(require 'dim-packages)

(when-running-macosx (require 'dim-init-macosx))
(when-running-debian-or-ubuntu (require 'dim-init-debian))

;; that's tapoueh.org git depo, but still named dim-muse.el
(require 'dim-muse)

;; that's local projects, and some other local muse sites
(require 'dim-projects)

;; configuration rcirc, and global shortcut to connect to servers
(require 'dim-rcirc)
(require 'dim-splits)

;; bindings --- depends on rcirc being loaded
(require 'dim-escreen)
(require 'dim-keys)
(require 'dim-modes)
(require 'dim-tels-pro)

;; empower M-term to remote hosts
(require 'cssh)

;; local utilities
(when (and (get-domain-name)
	   (string-match "hi-media-techno" (get-domain-name)))
  (require 'dim-check-prefix)
  (require 'dim-hi-media-vpn))

;; and the session
(setq desktop-restore-eager 20)
(desktop-save-mode 1)
(savehist-mode 1)
