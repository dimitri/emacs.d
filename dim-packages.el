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
 ;; (setq magit-git-executable "/usr/local/git/bin/git")

 ;; new setup, mac + debian VM for the "heavy" work
 (let ((svn "usr/bin/svn")
       (darcs "~/.cabal/bin/darcs"))
   (when (file-executable-p svn) (setq el-get-svn svn))
   (when (file-executable-p darcs) (setq el-get-darcs darcs))))

;; where to find init-package.el files
(setq el-get-user-package-directory "~/dev/emacs.d/packages.d")

;; github seems not to work anymore in http, require https
(setq el-get-github-default-url-type 'https)

;; personal recipes
(setq el-get-sources
      '((:name el-get :branch "master")

	(:name expand-region
	       :before (global-set-key (kbd "C-@") 'er/expand-region))

	(:name deft
	       :before (progn
                         (setq deft-default-extension "md")
                         (setq deft-directory "~/dev/emacs.d/notes")))

	;; (:name anything
	;;        :features anything-config
	;;        :before (global-set-key (kbd "M-s a") 'dim:anything-occur)
	;;        :after  (setq w3m-command nil))

	;; (:name descbinds-anything
	;;        :after (progn
	;; 		(descbinds-anything-install)
	;; 		(global-set-key (kbd "C-h b") 'descbinds-anything)))

	(:name vkill
	       :checksum fbaf37ba613a661eb46e3e380d72be8da0277cd0)

	(:name goto-last-change
	       :before (global-set-key (kbd "C-x C-/") 'goto-last-change))

	(:name popwin
	       :load-path ("." "misc")
	       :before (setq display-buffer-function 'popwin:display-buffer))

	(:name adoc-mode
	       :before (setq adoc-insert-replacement nil))

	;; (:name hide-region
	;;        :features hide-region
	;;        :before (progn
	;; 		 (global-set-key (kbd "C-c h h") 'hide-region-hide)
	;; 		 (global-set-key (kbd "C-c h r") 'hide-region-hide)
	;; 		 (global-set-key (kbd "C-c h u") 'hide-region-unhide)))

	;; (:name pgdevenv-el
	;;        :before (setq pgdev-ccache-path "/usr/local/bin/ccache"))

	;; (:name main-line
	;;        :before (setq main-line-separator-style 'arrow))

        (:name json-mode
               :features json-mode
               :after (define-key json-mode-map (kbd "M-q") 'json-mode-beautify))

	(:name switch-window
	       :before (progn
			 (global-set-key (kbd "C-x o") 'switch-window)
			 (global-set-key (kbd "C-x 9") 'delete-other-window)))

	(:name cssh
	       :after (cssh-define-global-bindings))))

;; my packages
(setq dim-packages
      (append
       ;; list of packages we use straight from official recipes
       '(
	 ;; anything
	 ;; elisp-slime-nav
	 ;; emacs-goodies-el
	 ;; php-mode-improved
	 git-gutter
	 go-mode
	 markdown-mode
	 pgsql-linum-format
         ;; auto-dictionary
         ;; bbdb
         ;; crontab-mode
         ;; deft
         ;; descbinds-anything
         ;; dpans2texi
         ;; geiser
         ;; google-maps
         ;; keywiz
         ;; magit ;;; installed via M-x package-list-packages
         ;; magit-view-file
         ;; magnus
         ;; mbsync
         ;; multiple-cursors
         ;; muse
         ;; protobuf-mode
         ;; psvn
         ;; redshank
         asciidoc
         browse-kill-ring
         color-theme-solarized
         color-theme-tango-2
         diminish
         dockerfile-mode
         eshell-manual
         go-eldoc
         icomplete+
         pandoc-mode
         paredit
         pcmpl-git
         powerline
         rainbow-delimiters
         rect-mark
         sicp
         smartparens
         smex
         switch-window
         vkill
         xcscope
	 wcheck-mode)

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

;; for notify, apt-get install libnotify-bin
(when-running-debian-or-ubuntu
 (loop for p in '(emms notify)		; verbiste?
       do (add-to-list 'dim-packages p)))

;; we dont' use mailq not emacs-w3m anymore, do we?
(when-running-macosx
 (loop for p in '(htmlize)		; emacs-w3m mailq
       do (add-to-list 'dim-packages p)))

;; in windows either, no mailq anymore
(when-running-windows
 (loop for p in '(naquadah-theme)	; mailq
       do (add-to-list 'dim-packages p)))

(el-get 'sync dim-packages)

;;;
;;; magit is now installed with M-x package
;;;
(require 'init-magit)

(provide 'dim-packages)
