;; Auteur : Dimitri Fontaine <dim@tapoueh.org>
;;
;; ce fichier permet de configurer emacs et xemacs

;; le load path, on a des choses en plus dans ~/.elisp
(setq load-path (cons "~/.elisp" load-path))

;; les modules additionnels dans .emacs.d
;; on veut charger quelques modules suppl�mentaires
(defun dim:add-my-extra-load-paths ()
  "define a list of paths to add to load-path and add each of them"
  (let ((dim:paths '("~/.elisp"
		     "~/.emacs.d"
		     "~/dev/elisp"
		     "~/dev/muse"
		     "~/dev/elisp/rcirc"
		     "~/.elisp/egg/egg"
		     "~/.elisp/magit/mainline/"
		     "~/.elisp/nxhtml/nxhtml"
		     "~/.elisp/skype/skype"
		     )))
    (dolist (path dim:paths)
      (setq load-path (cons path load-path)))))

;; extension du load-path proprement dite.
(dim:add-my-extra-load-paths)

;; useful wrappers
(setq chk:this-is-gnuemacs   (string-match "GNU Emacs" (emacs-version)))
(setq chk:this-is-xemacs     (string-match "XEmacs" (emacs-version)))

;; if it is emacs 21 or more recent, no toolbar
(if (>= emacs-major-version 21) (tool-bar-mode 0))

;; On text mode (and while editing mail) I don't want the emacs menu
(progn
  (if (not (eq window-system 'x))
      (menu-bar-mode nil)))

;; set some options for emacs
(progn
  (if chk:this-is-gnuemacs
      (progn
	;; the colors
	(if (eq window-system 'x)
	    (progn
	      ;; couleurs
	      ;(set-foreground-color "Black")
	      ;(set-background-color "lightgray")
	      ;(set-cursor-color "Black")
	      ;(set-border-color "DarkSlateBlue")

	      ;; th�me Tango, much better
	      (require 'color-theme)
	      (require 'color-theme-tango)
	      (color-theme-tango)

	      ;; taille et positionnement
	      (set-frame-position (selected-frame) 150 150)
	      (set-frame-size (selected-frame) 160 52)
	      
	      ;; set the font -- now see .Xresource
              ;(set-face-font 'default' "-misc-vgathin-medium-r-normal--16-16-75-75-c-90-iso8859-15" nil)
              ;(set-face-font 'default' "terminus-iso8859-15-14" nil)
              ;(set-face-font 'default' "terminus-20" nil)
              ;(set-face-font 'default' "terminus-iso8859-15-20" nil)
	      ;(set-face-font 'default' "-artwiz-smoothansi-*-*-*-*-*-*-*-*-*-*-*-*")

	      ;; emacs23 supports any X font
	      ;;(if (< emacs-major-version 23)
	      ;;  (set-face-font 'default' "-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso8859-15")
              ;; (set-face-font 'default' "Monospace-9"))

	      ;; Zoom in/out like feature, with mouse wheel
	      (global-set-key '[C-mouse-4] 'text-scale-increase)
	      (global-set-key '[C-mouse-5] 'text-scale-decrease)

	      ;; we don't want menu, we don't want scrollbar ... so
	      ;; why xwindow ?  note: the menu-bar-mode just want the
	      ;; 0 (zero) argument ...
	      ;(menu-bar-mode 1)
	      (menu-bar-mode nil)
	      (scroll-bar-mode nil)

	      (line-number-mode 1)
	      (column-number-mode 1)
	      (setq-default fill-column 76)
	      (setq auto-fill-mode 1)

	      (global-hl-line-mode 1))
	  )

	;; Syntax Highlight
	;(setq font-lock-maximum-decoration t
	;      font-lock-maximum-size nil)
	(require 'font-lock)
	(global-font-lock-mode 1)

	;;pour que la r�gion s�lectionn�e soit mise en surbrillance
	;(setq font-lock-maximum-size nil)
	(transient-mark-mode 1)

	;; some useful key bindings
	(global-set-key [home] (lambda () 
				 (interactive)
				 (if (and (bolp) key-home-jump)
				     (beginning-of-buffer)
				   (beginning-of-line))))

	(global-set-key [end] (lambda ()
				(interactive)
				(if (and (eolp) key-home-jump)
				    (end-of-buffer)
				  (end-of-line))))

	(global-set-key [delete] 'delete-char)
	(global-set-key [M-right] 'forward-word)
	(global-set-key [M-left] 'backward-word)
	(global-set-key [C-home] 'beginning-of-buffer)
	(global-set-key [C-end] 'end-of-buffer)
	;; (global-set-key [M-g] 'goto-line)
	(global-set-key "\M-g" 'goto-line)

	;; La molette, hop hop
	(global-set-key '[mouse-4] '(lambda () (interactive) (scroll-down 5)))
	(global-set-key '[mouse-5] '(lambda () (interactive) (scroll-up 5))))))

;; mes projects locaux
(require 'hm-projects)
(require 'dim-muse)

;; configuration rcirc, and global shortcut to connect to servers
(require 'dim-rcirc)

;; fonctions pratiques pour spliter la fen�tre courante et faire les choses
;; habituelles, comme d�marrer un term � droite.
(require 'dim-splits)

; on utilise ibuffer
(require 'ibuffer) 
(global-set-key "\C-x\C-b" 'ibuffer) 
(iswitchb-mode)

; find-file-at-point quand �a a du sens
(setq ffap-machine-p-known 'accept) ; no pinging
(setq ffap-url-regexp nil)          ; disable URL features in ffap
(setq ffap-ftp-regexp nil)          ; disable FTP features in ffap
(define-key global-map (kbd "C-x C-f") 'find-file-at-point)

; windmove permet de bouger dans les windows d'une frame avec Shift+fl�ches
(windmove-default-keybindings)

; dired-x pour ouvrir dired directement au bon endroit avec C-x C-j
(require 'dired-x)

; Hippie Expand pour un meilleur M-/ (noms de fichiers)
(require 'hippie-exp) 
(global-set-key (kbd "M-/") 'hippie-expand) 

; highlight de la colonne 80, toujours, et C-c m pour changer le 80 /
; activer le mode
;(require 'column-marker)
;(column-marker-1 80)
;(global-set-key [?\C-c ?m] 'column-marker-1)

;; Backuper les fichiers dans ~/.elisp/backups
(setq backup-directory-alist (quote ((".*" . "~/.elisp/backups/"))))

;; gestion de sessions : on red�marre en ouvrant tout comme avant
;; et on enregistre l'historique du minibuffer aussi
(when (= emacs-major-version 23)
  (progn
    (setq desktop-restore-eager 20)
    (desktop-save-mode 1)
    (savehist-mode 1)))

; winner-mode pour revenir sur le layout pr�c�dent
(winner-mode 1)

;;;
;;; Language modes
;;;

;; PHP mode
;; (autoload 'php-mode "php-mode" "PHP editing mode" t)
;; (setq auto-mode-alist (append '(("\\.php3" . php-mode))
;;			      '(("\\.php" . php-mode))
;;			      '(("\\.pinc" . php-mode))
;;			      '(("\\.p3" . php-mode))
;;			      auto-mode-alist))
;; On a nxhtml
;; (require 'php-mode)

;; CSS mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist       
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Use nxml-mode for smarty & cheetah template files, .tpl & .tmpl
(setq auto-mode-alist       
      ;(cons '("\\.tpl\\'" . html-mode) auto-mode-alist))
      (append '(("\\.tpl$" . nxml-mode)
		("\\.tmpl$" . nxml-mode)) auto-mode-alist))

;; reST mode
(autoload 'rst-mode
  "rst-mode" "mode for editing reStructuredText documents" t)
(require 'rst)
;;(add-hook 'text-mode-hook 'rst-text-mode-bindings)
(add-hook 'rst-mode-hook 'flyspell-mode)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode)) auto-mode-alist))

;; AsciiDoc mode
(require 'doc-mode)
(autoload 'doc-mode "doc-mode")

;; Egg mode, pour status git
(require 'egg)

;; Magit mode
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; nxhtml pour PHP/HTML
(load "~/.elisp/nxhtml/nxhtml/autostart.el")
(toggle-debug-on-error)
(custom-set-faces 
 '(mumamo-background-chunk-submode ((((class color)
				      (min-colors 88)
				      (background dark)) nil)))
 '(mumamo-background-chunk-major ((((class color)
				    (min-colors 88) 
				    (background dark)) nil))))

;; On pr�cise � M-x woman de ne pas ouvrir sa propre frame
(setq woman-use-own-frame nil)
(global-set-key (kbd "C-c w") 'woman)

;; Mon adresse email
(setq user-mail-address "dim@tapoueh.org")

(require 'cssh)

;; attention aux lignes trop longues dans les term
(add-hook 'term-mode-hook 
	  (lambda () (setq truncate-lines t)))

(add-hook 'term-mode-hook 
	  (lambda () (hl-line-mode -1)))

;;(add-hook 'term-mode-hook 
;;	  (lambda () (transient-mark-mode nil)))

;; Muse documentation
(require 'info)
(setq Info-additional-directory-list
      (cons (expand-file-name "~/dev/muse")
            Info-additional-directory-list))

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; dict
(setq dictionary-coding-systems-for-dictionaries '(("robert" . iso-8859-15)))
(setq dictionary-server "localhost")

;; my keys
(require 'dim-keys)

;; Skype
(require 'skype)
(setq skype--my-user-handle "dimitri-fontaine-himedia")

;; OfflineIMAP
(require 'dim-offlineimap)

;; Lua
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; PostgreSQL
(add-hook 'c-mode-hook
	  (function
	   (lambda nil 
	     (if (and buffer-file-name
		      (string-match "postgresql/src" buffer-file-name))
		 (progn
		   (c-set-style "bsd")
		   (setq c-basic-offset 4) 
		   (setq tab-width 4)
		   (c-set-offset 'case-label '+)
		   (setq indent-tabs-mode t)
		   )
	       ))))

;; optimisation surtout b�n�fique � Tramp
(setq vc-handled-backends nil)
