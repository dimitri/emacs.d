;;; dim-packages.el --- Dimitri Fontaine
;;
;; Set el-get-sources and call el-get to init all those packages we need.
;;
;; Use el-get to init most things, including local packages.
;; We trick by adding local recipes to the el-get-recipe-path.
;;
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/dev/emacs/el-get/recipes")
(setq el-get-verbose t)

;; some special for magit under MacOSX
(when-running-macosx
 (setq magit-git-executable "/usr/local/git/bin/git")
 (setq el-get-svn   "/usr/bin/svn")
 (setq el-get-darcs "~/.cabal/bin/darcs"))

;; where to find init-package.el files
(setq el-get-user-package-directory "~/dev/emacs.d/packages.d")

;; personal recipes
(setq el-get-sources
      '((:name el-get :branch "master")

	(:name magit
	       :before (lambda ()
			 (global-set-key (kbd "C-x C-z") 'magit-status)))

	(:name deft
	       :before (lambda ()
			 (setq deft-extension "muse")
			 (setq deft-directory "~/dev/emacs.d/notes")
			 (setq deft-text-mode 'muse-mode)))

	(:name anything
	       :features anything-config
	       :before (lambda ()
			 (global-set-key (kbd "M-s a") 'dim:anything-occur))
	       :after (lambda ()
			(setq w3m-command nil)))

	(:name descbinds-anything
	       :after (lambda ()
			(descbinds-anything-install)
			(global-set-key (kbd "C-h b") 'descbinds-anything)))

	(:name vkill
	       :checksum fbaf37ba613a661eb46e3e380d72be8da0277cd0)

	(:name goto-last-change
	       :before (lambda ()
			 (global-set-key (kbd "C-x C-/") 'goto-last-change)))

	(:name cssh
	       :after (lambda () (cssh-define-global-bindings)))))

(when-running-debian-or-ubuntu
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '((:name mailq
		:after (lambda () (mailq-modeline-display)))
	 (:name dictionary   :type elpa  :after 'dim:setup-package-dictionary)
	 (:name apel         :type apt-get))))

(when-running-macosx
 (mapc (lambda (source) (add-to-list 'el-get-sources source))
       '(;; (:name dictionary   :type elpa   :after 'dim:setup-package-dictionary)
	 (:name aspell-fr    :type fink)
	 (:name aspell-en    :type fink))))

;; my packages
(setq dim-packages
      (append
       ;; list of packages we use straight from official recipes
       '(nognus bbdb switch-window vkill google-maps
		offlineimap asciidoc smex geiser xcscope
		anything descbinds-anything
		emms emacs-goodies-el sicp auto-dictionnary keywiz
		pgsql-linum-format psvn rect-mark crontab-mode icomplete+
		php-mode-improved rainbow-delimiters muse deft
		color-theme-solarized protobuf-mode)

       ;; add to my packages all from `el-get-sources'
       ;; (loop for src in el-get-sources
       ;; 	     for name = (el-get-as-symbol (el-get-source-name src))
       ;; 	     unless (member name '(emms))
       ;; 	     collect name)))
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(when-running-debian-or-ubuntu
 (loop for p in '(notify verbiste)
       do (add-to-list 'dim-packages p)))

(when-running-macosx
 (loop for p in '(htmlize emacs-w3m mailq)
       do (add-to-list 'dim-packages p)))

(when-running-windows
 (loop for p in '(naquadah-theme mailq)
       do (add-to-list 'dim-packages p)))

(el-get 'sync dim-packages)

(provide 'dim-packages)
