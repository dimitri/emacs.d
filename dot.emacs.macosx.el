;;; dim .emacs
;;
;; les modules additionnels dans .emacs.d
;; on veut charger quelques modules supplémentaires
(defun dim:add-my-extra-load-paths ()
  "define a list of paths to add to load-path and add each of them"
  (let ((dim:paths '("~/.emacs.d"
		     "~/dev/elisp"
		     "~/dev/muse"
		     "~/dev/magit/mainline"
		     "~/.emacs.d/egg/egg"
		     "~/.emacs.d/color-theme-6.6.0"
		     "~/.emacs.d/color-theme-6.6.0/themes"
		     "~/.emacs.d/muse/muse/lisp"
		     "~/.emacs.d/rcirc"
		     "~/.emacs.d/mew/mew-6.2"
		     "~/.emacs.d/nxhtml"
		     ;;"~/.emacs.d/emacs-jabber-0.7.1"
		     "~/.emacs.d/dictionary-1.8.7"
		     "~/.emacs.d/w3m/emacs-w3m/"
		     )))
    (dolist (path dim:paths)
      (setq load-path (cons path load-path)))))

;; extension du load-path proprement dite.
(dim:add-my-extra-load-paths)

;; Sous mac plein de softs intéressants sont dans /sw/bin
(setenv "PATH" (concat (getenv "PATH") ":" "/sw/bin"))
(add-to-list 'exec-path "/sw/bin")

;; par défaut on fait tout en UTF-8
(prefer-coding-system 'utf-8)

;; chuuut
(setq ring-bell-function 'ignore)

;; thème Tango, much better
(require 'color-theme)
;(require 'color-theme-tango)
;(load-library "color-theme-library")
;(color-theme-tango)
;(color-theme-tango-2)
;(color-theme-scintilla)
;(color-theme-aliceblue)

;(require 'color-theme-zenburn)
;(color-theme-zenburn)

(require 'color-theme-emacs23-default)
(color-theme-emacs23-default)

;; MacOSX specific setting
(setq mac-allow-anti-aliasing t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
(ns-set-background-alpha 0.9)

;; font, taille et position
(if (or (eq 0 (string-match "^23.1.50" emacs-version))
	(eq 0 (string-match "^23.0.9[24]" emacs-version)))
    (progn
      (set-face-font 'default "Monaco-12")
      (set-frame-position (selected-frame) 60 30)
      ;(set-frame-size (selected-frame) 165 45))
      ;(set-frame-size (selected-frame) 180 55))
      (set-frame-size (selected-frame) 192 54))

  ;; older emacs (23.0.90) didn't share same fonts rendering
  (set-frame-position (selected-frame) 90 45)
  (set-frame-size (selected-frame) 170 45))

;; configuration rcirc
(require 'dim-rcirc)

;; bindings --- depends on rcirc being loaded
(require 'dim-keys)

;; chargement des projets locaux
(require 'my-projects)

;; on utilise ibuffer
(require 'ibuffer)
(global-set-key "\C-x\C-b" 'ibuffer)
(iswitchb-mode)

;; on veut voir la sélection en cours, et aussi la ligne en cours d'édition
(transient-mark-mode 1) 
(global-hl-line-mode 1)

;; réglages de colonnes
(column-number-mode 1)
(setq-default fill-column 76)

;; pas de tool bar, pas de scroll bar merci
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;; gestion de session
(desktop-save-mode 1)

; winner-mode pour revenir sur le layout précédent
(winner-mode 1)

;; elscreen
;(require 'elscreen)
;(global-set-key (kbd "C-(") '(lambda () (interactive) (elscreen-previous)))
;(global-set-key (kbd "C-)") '(lambda () (interactive) (elscreen-next)))))

;; Backuper les fichiers dans ~/.elisp/backups
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; attention aux lignes plus longues que la window dans term.el
(add-hook 'term-mode-hook (lambda () (setq truncate-lines t)))
;;(add-hook 'term-mode-hook (lambda () (transient-mark-mode nil)))

;; cssh permet d'avoir clusterssh pour emacs, et C-= pour ouvrir un remote
;; term
;; module local
(require 'cssh)

;; fonctions pratiques pour spliter la fenêtre courante et faire les choses
;; habituelles, comme démarrer un term à droite.
(require 'dim-splits)

;; muse projects
(require 'dim-muse)

;; TRAMP
;;(setq tramp-default-method "scpc")
;;(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")

;; On précise à M-x woman de ne pas ouvrir sa propre frame
(setq woman-use-own-frame nil)

;; xlhtml modes (php / html), which turns on toggle-debug-on-error
(load "~/.emacs.d/nxhtml/autostart.el")
(setq debug-on-error nil)

;; asciidoc mode support
(require 'asciidoc)
(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-hook 'doc-mode-hook '(lambda ()
			    (turn-on-auto-fill)
			    (require 'asciidoc)))

;; Magit!
(require 'magit)
(setq magit-git-executable "/sw/bin/git")
(global-set-key (kbd "C-x g") 'magit-status)

;; egg: Emacs Got Git! (magit fork)
(require 'egg)
(setq egg-git-command/"sw/bin/git")

;; dict mode
(load "~/.emacs.d/dictionary-1.8.7/dictionary-init.el")
(global-set-key (kbd "C-c ?") (lambda () (interactive) 
				(dictionary-lookup-definition)))

;; w3m
(require 'w3m-load)

;; PostgreSQL source code
(add-hook 'c-mode-hook
	  (function
	   (lambda nil 
	     (if (string-match "pgsql" buffer-file-name)
		 (progn
		   (c-set-style "bsd")
		   (setq c-basic-offset 4) 
		   (setq tab-width 4)
		   (c-set-offset 'case-label '+)
		   (setq indent-tabs-mode t)
		   )
	       ))))

;;; To work on the documentation, the following (or a variant, as above)
;;; can be helpful.

(defun pgsql-sgml-mode ()
  "SGML mode adjusted for PostgreSQL project"
  (interactive)
  (sgml-mode)
  (setq sgml-basic-offset 1))

(setq auto-mode-alist
  (cons '("\\(postgres\\|pgsql\\).*\\.sgml\\'" . pgsql-sgml-mode)
        auto-mode-alist))