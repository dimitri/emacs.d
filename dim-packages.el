;;; dim-packages.el --- Dimitri Fontaine
;;
;; Set el-get-sources and call el-get to init all those packages we need.
;;
;; Use el-get to init most things, including local packages.
;; We trick by adding local recipes to the el-get-recipe-path.
;;
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/dev/emacs/el-get/recipes")
(setq el-get-status-file "~/dev/emacs.d/el-get-status.el")

;; some special for magit under MacOSX
(when-running-macosx
 (setq magit-git-executable "/usr/local/git/bin/git")
 (setq el-get-svn   "/usr/bin/svn")
 (setq el-get-darcs "~/.cabal/bin/darcs"))

;; personal recipes
(setq el-get-sources
      '((:name smex
	       :after (lambda ()
			(setq smex-save-file "~/.emacs.d/.smex-items")
			(global-set-key (kbd "ESC M-x") 'execute-extended-command)
			(global-set-key (kbd "M-x") 'smex)
			(global-set-key (kbd "M-X") 'smex-major-mode-commands)))

	(:name magit
	       :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))

	(:name asciidoc
	       :type elpa
	       :after (lambda ()
			(autoload 'doc-mode "doc-mode" nil t)
			(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
			(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))
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

	(:name geiser
	       :after (lambda ()
			(setq geiser-guile-binary "guile-2.0")
			(setq geiser-repl-use-other-window nil)
			(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
			(setq geiser-active-implementations '(guile))))))

(defun dim:setup-package-dictionary ()
  "That's called from two places, give it a name"
  (require 'dictionary)

  ;; (make-local-hook HOOK)
  ;; This function is obsolete since 21.1;
  ;; not necessary any more.
  (defun make-local-hook (name) "backward compat" nil)

  (setq dictionary-coding-systems-for-dictionaries '(("robert" . iso-8859-15)))
  (unless (string-match "apple-darwin" system-configuration)
    (setq dictionary-server "localhost")))

(when-running-debian-or-ubuntu
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '((:name mailq
		:after (lambda () (mailq-modeline-display)))

	 (:name dictionary-el    :type apt-get   :after 'dim:setup-package-dictionary)
	 (:name apel             :type apt-get)
	 (:name muse-el          :type apt-get))))

(when-running-macosx
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '((:name htmlize      :type elpa)
	 (:name dictionary   :type elpa   :after 'dim:setup-package-dictionary)
	 (:name aspell-fr    :type fink)
	 (:name aspell-en    :type fink))))

;; my packages
(setq dim-packages
      (append
       ;; list of packages we use straight from official recipes
       '(nognus bbdb cssh el-get switch-window vkill google-maps
		verbiste sicp emacs-goodies-el notify
		auto-dictionnary keywiz git-commit-mode
		pgsql-linum-format lua-mode python psvn rect-mark
		crontab-mode icomplete+ php-mode-improved
		rainbow-delimiters)

       ;; add to my packages all from `el-get-sources'
       (loop for src in el-get-sources collect (el-get-source-name src))))

(when-running-macosx
 (loop for p in '(emacs-w3m muse mailq) do (add-to-list 'dim-packages p)))

(el-get 'sync dim-packages)

(provide 'dim-packages)
