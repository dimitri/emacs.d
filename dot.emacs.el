;; Auteur : Dimitri Fontaine <dim@tapoueh.org>
;;
;; ce fichier permet de configurer emacs et xemacs

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
	      (set-foreground-color "Black")
	      (set-background-color "lightgray")
	      (set-cursor-color "Black")
	      (set-border-color "DarkSlateBlue")

	      ;; taille et positionnement
	      (set-frame-position (selected-frame) 150 150)
	      (set-frame-size (selected-frame) 164 50)
	      
	      ;; set the font -- now see .Xresource
              ;(set-face-font 'default' "-misc-vgathin-medium-r-normal--16-16-75-75-c-90-iso8859-15" nil)
              ;(set-face-font 'default' "terminus-iso8859-15-14" nil)
              ;(set-face-font 'default' "terminus-20" nil)
              ;(set-face-font 'default' "terminus-iso8859-15-20" nil)

	      ;; emacs23 supports any X font
	      (if (< emacs-major-version 23)
		  (set-face-font 'default' "-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso8859-15")
		(set-face-font 'default' "Monospace-10"))

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

	;;pour que la région sélectionnée soit mise en surbrillance
	;(setq font-lock-maximum-size nil)
	(transient-mark-mode t) 

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

;; le load path, on a des choses en plus dans ~/.elisp
(setq load-path (cons "~/.elisp" load-path))

;; mes projects locaux
(require 'hm-projects)
(require 'dim-muse)

; on utilise ibuffer
(require 'ibuffer) 
(global-set-key "\C-x\C-b" 'ibuffer) 
(iswitchb-mode)

; windmove permet de bouger dans les windows d'une frame avec Shift+flèches
(windmove-default-keybindings)

; dired-x pour ouvrir dired directement au bon endroit avec C-x C-j
(require 'dired-x)

; Hippie Expand pour un meilleur M-/ (noms de fichiers)
(require 'hippie-exp) 
(global-set-key (kbd "M-/") 'hippie-expand) 

; highlight de la colonne 80, toujours, et C-c m pour changer le 80 /
; activer le mode
(require 'column-marker)
(column-marker-1 80)
(global-set-key [?\C-c ?m] 'column-marker-1)

;; Backuper les fichiers dans ~/.elisp/backups
(setq backup-directory-alist (quote ((".*" . "~/.elisp/backups/"))))

;; gestion de sessions : on redémarre en ouvrant tout comme avant
;; et on enregistre l'historique du minibuffer aussi
(when (= emacs-major-version 23)
  (progn
    (desktop-save-mode 1)
    (savehist-mode 1)))

;; on utilise aussi elscreen, avec support de la molette dans la barre de
;; titre s'il vous plaît
;; mais seulement dans emacs23
(when (= emacs-major-version 23)
  (progn
    (require 'elscreen)
    (global-set-key (kbd "<header-line> <mouse-4>")
		    '(lambda () (interactive) (elscreen-previous)))
    (global-set-key (kbd "<header-line> <mouse-5>")
		    '(lambda () (interactive) (elscreen-next)))
    (global-set-key (kbd "C-z SPC") '(lambda () (interactive) (elscreen-next)))))

;;;
;;; Language modes
;;;

;; PHP mode
(autoload 'php-mode "php-mode" "PHP editing mode" t)
(setq auto-mode-alist (append '(("\\.php3" . php-mode))
			      '(("\\.php" . php-mode))
			      '(("\\.pinc" . php-mode))
			      '(("\\.p3" . php-mode))
			      auto-mode-alist))
(require 'php-mode)

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

;; On précise à M-x woman de ne pas ouvrir sa propre frame
(setq woman-use-own-frame nil)

;; Mon adresse email
(setq user-mail-address "dim@tapoueh.org")

(require 'cssh)

;; attention aux lignes trop longues dans les term
(add-hook 'term-mode-hook 
	  (lambda () (setq truncate-lines t)))

;; C-c r pour revert-buffer
(global-set-key (kbd "C-c r") '(lambda () (interactive) (revert-buffer)))
