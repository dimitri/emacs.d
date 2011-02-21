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
;; Use el-get to init most things, including local packages.
;; We trick by adding local recipes to the el-get-recipe-path.
;;
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/dev/emacs/el-get/recipes")

;; some special for magit under MacOSX
(when-running-macosx
 (setq magit-git-executable "/usr/local/git/bin/git")
 (setq el-get-svn   "/usr/bin/svn")
 (setq el-get-darcs "~/.cabal/bin/darcs"))

(setq el-get-sources

      '(cssh el-get switch-window vkill google-maps verbiste
	      mailq sicp emacs-goodies-el notify auto-dictionnary
	      keywiz git-commit-mode pgsql-linum-format lua-mode
	      python psvn rect-mark

	(:name magit
	       :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))

	(:name asciidoc
	       :type elpa
	       :after (lambda ()
			(autoload 'doc-mode "doc-mode" nil t)
			(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
			(add-hook 'doc-mode-hook '(lambda ()
						    (turn-on-auto-fill)
						    (require 'asciidoc)))))

	(:name emms
	       :after (lambda ()
			(emms-standard)
			;; (emms-default-players) ; I want VLC mainly
			(setq emms-player-list
			      '(emms-player-vlc
				emms-player-mpg321
				emms-player-ogg123
				emms-player-mplayer-playlist
				emms-player-mplayer))

			;; Show the current track each time EMMS
			;; starts to play a track with "NP : "
			(add-hook 'emms-player-started-hook 'emms-show)
			(setq emms-show-format "EMMS Now Playing: %s")

			(add-hook 'dired-load-hook
				  (define-key dired-mode-map (kbd "E") 'emms-play-dired))))

	(:name nxhtml
	       :after (lambda ()
			(setq debug-on-error nil)
			(setq mumamo-chunk-coloring 1)
			(custom-set-faces
			 '(mumamo-background-chunk-submode1 ((((class color)
							       (min-colors 88)
							       (background dark)) nil)))
			 '(mumamo-background-chunk-submode2 ((((class color)
							       (min-colors 88)
							       (background dark)) nil)))
			 '(mumamo-background-chunk-submode3 ((((class color)
							       (min-colors 88)
							       (background dark)) nil)))
			 '(mumamo-background-chunk-submode4 ((((class color)
							       (min-colors 88)
							       (background dark)) nil)))
			 '(mumamo-background-chunk-major ((((class color)
							    (min-colors 88)
							    (background dark)) nil))))))

	(:name xcscope
	       :after (lambda ()
			;; cscope debian package includes cscope-indexer, no luck under MacOSX
			(setq cscope-indexing-script
			      (if (file-executable-p "/usr/bin/cscope-indexer")
				  "/usr/bin/cscope-indexer"
				"~/bin/cscope-indexer"))))

	(:name css-mode
	       :type elpa
	       :after (lambda ()
			(autoload 'css-mode "css-mode")
			(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))))

	(:name hl-sexp
	       :after (lambda ()
			;;(set-face-attribute 'hl-sexp-face nil :background "RosyBrown1")
			;;(set-face-attribute 'hl-sexp-face nil :background "LightGoldenRod")
			(set-face-attribute 'hl-sexp-face nil :background "LightYellow")))

	(:name offlineimap
	       :after (lambda ()
			(require 'gnus-load)
			(require 'gnus)
			(setq offlineimap-enable-mode-line-p
			      '(member major-mode '(offlineimap-mode
						    gnus-group-mode
						    gnus-summary-mode)))
			(setq offlineimap-mode-line-symbol "♻")
			(setq offlineimap-timestamp "%k:%M:%S ")
			(loop with color = "DarkGoldenrod"
			      for face in '(offlineimap-msg-syncingfolders-face
					    offlineimap-msg-skippingfolder-face)
			      do (set-face-attribute face nil :foreground color))
			(define-key gnus-group-mode-map (kbd "O") 'offlineimap)))

	(:name goto-last-change
	       :after (lambda ()
			(global-set-key (kbd "C-x C-/") 'goto-last-change)))

	(:name gist            :type elpa)
	(:name lisppaste       :type elpa)))

(defun dim:setup-package-dictionary ()
  "That's called from two places, give it a name"
  (require 'dictionary)
  (setq dictionary-coding-systems-for-dictionaries '(("robert" . iso-8859-15)))
  (unless (string-match "apple-darwin" system-configuration)
    (setq dictionary-server "localhost")))

(when-running-debian-or-ubuntu
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '(nognus bbdb
	 (:name dictionary-el    :type apt-get   :after 'dim:setup-package-dictionary)
	 (:name apel             :type apt-get)
	 (:name muse-el          :type apt-get))))

(when-running-macosx
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '(nognus emacs-w3m bbdb
	 (:name htmlize      :type elpa)
	 (:name dictionary   :type elpa   :after 'dim:setup-package-dictionary)
	 (:name muse         :type elpa)
	 (:name aspell-fr    :type fink)
	 (:name aspell-en    :type fink))))

(el-get 'sync)

(provide 'dim-packages)
