;;; init.el --- Dimitri Fontaine
;;
;; load-path has to be explicitly setup first


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun dim:add-my-extra-load-paths (&optional paths)
  "define a list of paths to add to load-path and add each of them"
  (let ((dim:paths '(
                     "~/dev/emacs/el-get"
		     "~/dev/emacs.d"
		     "~/dev/emacs.d/rcirc"
		     "~/dev/emacs.d/lib"
		     "~/dev/emacs.d/fun"
		     "~/dev/emacs.d/packages.d"
		     ;; "~/dev/emacs/rcirc-groups"
		     ;; "~/dev/emacs/pgdevenv-el"
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
(require 'wget)

;; macosx specific path settings need to be set before el-get calls
(when-running-macosx
 (setenv "PATH"
	 (mapconcat 'identity
		    (list (getenv "PATH")
			  "~/bin"
			  "/usr/local/bin"
			  "/usr/local/sbin"
			  ;; "/usr/local/git/bin"
			  "/usr/local/texlive/2012/bin/x86_64-darwin"
			  "/Users/dim/Library/Haskell/bin") ":"))
 (add-to-list 'exec-path "~/bin")
 (add-to-list 'exec-path "/usr/local/bin")
 ;; (add-to-list 'exec-path "/usr/local/git/bin")
 (add-to-list 'exec-path "/usr/local/texlive/2012/bin/x86_64-darwin")
 (add-to-list 'exec-path "/Users/dim/Library/Haskell/bin"))

(setenv "PKG_CONFIG_PATH" ":/usr/local/opt/libffi/lib/pkgconfig")

(setenv "EMACS" (concat invocation-directory invocation-name))

;; load slime early to avoid compat problems with some packages
(require 'dim-lisp)

;; now the external packages we depend upon
(require 'dim-packages)

;; those OS specific setup require the packages to be initialized
(when-running-macosx (require 'dim-init-macosx))
(when-running-windows (require 'dim-init-windows))
(when-running-debian-or-ubuntu (require 'dim-init-debian))

;; that's tapoueh.org git depo
;; (load "~/dev/tapoueh.org/tapoueh.el")

;; that's local projects, and some other local muse sites
;; (require 'dim-projects)

;; configuration rcirc, and global shortcut to connect to servers
;; (require 'dim-rcirc)

;; bindings --- depends on rcirc being loaded
(require 'dim-escreen)
(require 'dim-keys)
(require 'dim-modes)
(require 'dim-monip)

;; empower M-term to remote hosts
(require 'cssh)

;; and the session
(setq desktop-restore-eager 20
      desktop-lazy-verbose nil
      desktop-restore-frames nil
      ;; mostly default values, but at least controled
      desktop-restore-in-current-display nil
      desktop-restore-reuses-frames t
      desktop-restore-forces-onscreen t)
(desktop-save-mode 1)
(savehist-mode 1)

(defun dim:notify-startup-done ()
  " notify user that Emacs is now ready"
  (el-get-notify
   "Emacs is ready."
   (format "The init sequence took %g seconds."
	   (float-time (time-subtract after-init-time before-init-time)))))

(add-hook 'after-init-hook 'dim:notify-startup-done)

;; and start a server in case we might need it
(unless (or (daemonp) server-mode) (server-mode))

(put 'dired-find-alternate-file 'disabled nil)
