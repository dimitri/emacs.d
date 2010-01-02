;;; init.el --- Dimitri Fontaine
;;
;; load-path has to be explicitly setup first

(defun dim:add-my-extra-load-paths (&optional paths)
  "define a list of paths to add to load-path and add each of them"
  (let ((dim:paths '("~/dev/emacs.d"
		     "~/dev/emacs.d/rcirc"
		     "~/dev/emacs.d/lib"
		     "~/dev/emacs/cssh"
		     "~/dev/emacs/rcirc-groups"
		     "~/dev/tapoueh.org"
		     "~/.emacs.d/w3m/emacs-w3m/"
		     "~/.emacs.d/emms/lisp/"
		     )))
    (dolist (path (or paths dim:paths))
      (setq load-path (cons path load-path)))))

;; extension du load-path proprement dite.
(dim:add-my-extra-load-paths)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

(if (string-match "apple-darwin" system-configuration)
    (require 'dim-init-macosx)

  ;; beware of debian/kFreeBSD. Yet I intend to be using it.
  (when (string-match "Debian" (emacs-version))
    (require 'dim-init-debian)))

(require 'dim-visual-common)
(require 'dim-projects)

;; that's tapoueh.org git depo, but still named dim-muse.el
(require 'dim-muse)

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

;; and the session
(setq desktop-restore-eager 20)
;(desktop-save-mode 1)
(savehist-mode 1)
