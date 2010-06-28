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
		     "~/.emacs.d/magit"
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


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;
;; My attempts at managing packages
;;
(require 'el-get)

(setq el-get-sources
      '((:name jd
	       :type git
	       :url "git://git.naquadah.org/~jd/jd-el.git"
	       :features flyguess
	       :info nil
	       :build nil)

	(:name bbdb
	       :type git
	       :url "git://github.com/barak/BBDB.git"
	       :load-path ("./lisp" "./bits")
	       :info "texinfo"
	       :build ("./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs" "make"))

	(:name magit
	       :type git
	       :url "http://github.com/philjackson/magit.git"
	       :info "."
	       :build ("./autogen.sh" "./configure" "make"))

	(:name vkill
	       :type http
	       :url "http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el"
	       :features vkill)

	(:name asciidoc        :type elpa)
	(:name auto-dictionary :type elpa)
	(:name css-mode        :type elpa)
	(:name gist            :type elpa)
	(:name lua-mode        :type elpa)
	(:name lisppaste       :type elpa)))

(when-running-debian-or-ubuntu 
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '((:name dictionary-el   :type apt-get)
	 (:name muse            :type apt-get))))

(when-running-macosx
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '((:name htmlize         :type elpa)
	 (:name dictionary-el   :type elpa)
	 (:name muse            :type elpa))))
(el-get)

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

;; and the session
(setq desktop-restore-eager 20)
(desktop-save-mode 1)
(savehist-mode 1)
