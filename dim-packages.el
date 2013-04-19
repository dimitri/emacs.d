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

 ;; new setup, mac + debian VM for the "heavy" work
 (let ((svn "usr/bin/svn")
       (darcs "~/.cabal/bin/darcs"))
   (when (file-executable-p svn) (setq el-get-svn svn))
   (when (file-executable-p darcs) (setq el-get-darcs darcs))))

;; where to find init-package.el files
(setq el-get-user-package-directory "~/dev/emacs.d/packages.d")

;; personal recipes
(setq el-get-sources
      '((:name el-get :branch "master")

	(:name magit
	       :before (global-set-key (kbd "C-x C-z") 'magit-status))

	(:name expand-region
	       :before (global-set-key (kbd "C-@") 'er/expand-region))

	(:name deft
	       :before (progn
			 (setq deft-extension "muse")
			 (setq deft-directory "~/dev/emacs.d/notes")
			 (setq deft-text-mode 'muse-mode)))

	(:name anything
	       :features anything-config
	       :before (global-set-key (kbd "M-s a") 'dim:anything-occur)
	       :after  (setq w3m-command nil))

	(:name descbinds-anything
	       :after (progn
			(descbinds-anything-install)
			(global-set-key (kbd "C-h b") 'descbinds-anything)))

	(:name vkill
	       :checksum fbaf37ba613a661eb46e3e380d72be8da0277cd0)

	(:name goto-last-change
	       :before (global-set-key (kbd "C-x C-/") 'goto-last-change))

	(:name popwin
	       :load-path ("." "misc")
	       :before (setq display-buffer-function 'popwin:display-buffer))

	(:name adoc-mode
	       :before (setq adoc-insert-replacement nil))

	(:name pgdevenv-el
	       :before (setq pgdev-ccache-path "/usr/local/bin/ccache"))

	;; (:name main-line
	;;        :before (setq main-line-separator-style 'arrow))

	(:name cssh
	       :after (cssh-define-global-bindings))))

;; my packages
(setq dim-packages
      (append
       ;; list of packages we use straight from official recipes
       '(gnus bbdb switch-window vkill google-maps pgdevenv-el
	      mbsync asciidoc smex geiser xcscope multiple-cursors
	      anything descbinds-anything pcmpl-git magit-view-file
	      emacs-goodies-el sicp auto-dictionnary keywiz pandoc-mode
	      pgsql-linum-format psvn rect-mark crontab-mode icomplete+
	      php-mode-improved rainbow-delimiters muse deft dpans2texi
	      markdown-mode color-theme-solarized protobuf-mode paredit
	      git-gutter haskell-mode eshell-manual browse-kill-ring
	      elisp-slime-nav redshank color-theme-tango-2 powerline)

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

(provide 'dim-packages)
