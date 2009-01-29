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

	      ;; thème Tango, much better
	      (require 'color-theme-tango)
	      (color-theme-tango)

	      ;; taille et positionnement
	      (set-frame-position (selected-frame) 150 150)
	      (set-frame-size (selected-frame) 230 60)
	      
	      ;; set the font -- now see .Xresource
              ;(set-face-font 'default' "-misc-vgathin-medium-r-normal--16-16-75-75-c-90-iso8859-15" nil)
              ;(set-face-font 'default' "terminus-iso8859-15-14" nil)
              ;(set-face-font 'default' "terminus-20" nil)
              ;(set-face-font 'default' "terminus-iso8859-15-20" nil)
	      ;(set-face-font 'default' "-artwiz-smoothansi-*-*-*-*-*-*-*-*-*-*-*-*")

	      ;; emacs23 supports any X font
	      (if (< emacs-major-version 23)
		  (set-face-font 'default' "-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso8859-15")
		(set-face-font 'default' "Monospace-9"))

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

;; navigation dans les parenthèses
;; http://www.emacswiki.org/emacs/ParenthesisMatching
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'goto-match-paren)

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
    (global-set-key (kbd "C-(") '(lambda () (interactive) (elscreen-previous)))
    (global-set-key (kbd "C-)") '(lambda () (interactive) (elscreen-next)))))

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

(add-hook 'term-mode-hook 
	  (lambda () (transient-mark-mode nil)))

;; C-c r pour revert-buffer
(global-set-key (kbd "C-c r") '(lambda () (interactive) (revert-buffer)))

;; C-c d pour écrire la date
(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%Y%m%d" (current-time))))

(global-set-key "\C-cd" 'insert-date)

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

;; Custom window splitting shortcuts
;;  C-c 1  horizontal split, very little window on the bottom
;;  C-c 2  horizontal split, 3/4 of the space atop, 1/4 at the bottom
;;  C-c 3  vertical splitting of main window, right part horizontally split

(defun split-window-vertically-min-bottom ()
  "split current window vertically and select new window, 4 lines height"
  (interactive)
  (select-window (split-window-vertically -4)))

(defun split-window-vertically-quarter-bottom ()
  "split current window vertically and select new window, 1/4 of current window height"
  (interactive)
  (let* ((edges  (window-edges))
	 (top    (second edges))
	 (bottom (fourth edges))
	 (heigth (- bottom top)))
    (select-window (split-window-vertically (- (/ heigth 4))))))

(defun split-window-in-three ()
  "split current window horizontally then split new window vertically"
  (interactive)
  (select-window (split-window-horizontally 90))
  (split-window-vertically))

(global-set-key (kbd "C-c 1") 'split-window-vertically-min-bottom)
(global-set-key (kbd "C-c 2") 'split-window-vertically-quarter-bottom)
(global-set-key (kbd "C-c 3") 'split-window-in-three)

;; Add specific key mapping to term mode, as C-c 1 is already in use
(define-key term-raw-map (kbd "C-c c 1") 'split-window-vertically-min-bottom)
(define-key term-raw-map (kbd "C-c c 2") 'split-window-vertically-quarter-bottom)
(define-key term-raw-map (kbd "C-c c 3") 'split-window-in-three)
