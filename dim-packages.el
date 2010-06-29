;;; dim-packages.el --- Dimitri Fontaine
;;
;; Set el-get-sources and call el-get to init all those packages we need.
;;
;; First setup ELPA, which el-get depends upon

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

;; some special for magit under MacOSX
(when-running-macosx
 (setq magit-git-executable "/usr/local/git/bin/git"))

(setq el-get-sources
      '((:name jd
	       :type git
	       :url "git://git.naquadah.org/~jd/jd-el.git"
	       :features google-maps)

	(:name magit
	       :type git
	       :url "http://github.com/philjackson/magit.git"
	       :info "."
	       :build ("./autogen.sh" "./configure" "make"))

	(:name emms
	       :type git
	       :url "git://git.sv.gnu.org/emms.git"
	       :info "doc"
	       :build ("make"))

	(:name nxhtml
	       :type git
	       :url "http://github.com/emacsmirror/nxhtml.git"
	       :load "autostart.el")

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
       '((:name bbdb
		:type git
		:url "git://github.com/barak/BBDB.git"
		:load-path ("./lisp" "./bits")
		:info "texinfo"
		:build ("./configure" "make"))
	 (:name dictionary-el    :type apt-get)
	 (:name emacs-goodies-el :type apt-get)
	 (:name apel             :type apt-get)
	 (:name muse-el          :type apt-get))))

(when-running-macosx
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '((:name bbdb
		:type git
		:url "git://github.com/barak/BBDB.git"
		:load-path ("./lisp" "./bits")
		:info "texinfo"
		:build ("./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs" "make autoloads" "make"))

	 (:name htmlize      :type elpa)
	 (:name dictionary   :type elpa)
	 (:name muse         :type elpa))))
(el-get)

(provide 'dim-packages)
