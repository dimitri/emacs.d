;;; dim .emacs
;;
;; les modules additionnels dans .emacs.d
;; on veut charger quelques modules supplémentaires
(defun dim:add-my-extra-load-paths ()
  "define a list of paths to add to load-path and add each of them"
  (let ((dim:paths '("~/.emacs.d"
		     ;"~/dev/magit/mainline"
		     ;"~/.emacs.d/egg/egg"
		     "~/.emacs.d/color-theme-6.6.0"
		     "~/.emacs.d/muse/muse/lisp"
		     "~/.emacs.d/rcirc"
		     ;;"~/.emacs.d/emacs-jabber-0.7.1"
		     )))
    (dolist (path dim:paths)
      (setq load-path (cons path load-path)))))

;; extension du load-path proprement dite.
(dim:add-my-extra-load-paths)

;; entrer dans le debugger à la moindre erreur peut être pratique, ou
;; énervant.
(setq debug-on-error nil)

;; par défaut on fait tout en UTF-8
(prefer-coding-system 'utf-8)

;;
;; On charge reccursivement depuis le load-path
;(normal-top-level-add-subdirs-to-load-path)
;;(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;    (let* ((my-lisp-dir "~/.emacs.d/")
;;	   (default-directory my-lisp-dir))
;;      (setq load-path (cons my-lisp-dir load-path))
;;      (normal-top-level-add-subdirs-to-load-path)))

;; taille et position
(set-frame-position (selected-frame) 60 35)
(set-frame-size (selected-frame) 166 42)

;; couleurs
;;(set-foreground-color "Black")
;;(set-background-color "lightgray")
;;(set-cursor-color "Black")
;;(set-border-color "DarkSlateBlue")

;; thème Tango, much better
(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)
;(color-theme-tango-2)
;(color-theme-zenburn)

;; bindings
(global-set-key (kbd "C-c r") 'revert-buffer)

;; chargement des projets locaux
(require 'my-projects)

;; configuration rcirc, and global shortcut to connect to servers
(require 'dim-rcirc)
(global-set-key (kbd "C-c i") 'dim-rcirc-start)

;; on utilise ibuffer
(require 'ibuffer)
(global-set-key "\C-x\C-b" 'ibuffer)
(iswitchb-mode)

;; déplacements avec Shift-<flèche>
(windmove-default-keybindings)

;; dired-x pour C-x C-j
(require 'dired-x)

;; on veut voir la sélection en cours
(transient-mark-mode 1) 

;; réglages de colonnes
(column-number-mode 1)
(setq-default fill-column 76)

;; pas de tool bar, pas de scroll bar merci
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;; rendu des fontes
;;(setq mac-allow-anti-aliasing nil)

;; gestion de session
(desktop-save-mode 1)

; winner-mode pour revenir sur le layout précédent
(winner-mode 1)

;; elscreen
;(require 'elscreen)
;(global-set-key (kbd "C-(") '(lambda () (interactive) (elscreen-previous)))
;(global-set-key (kbd "C-)") '(lambda () (interactive) (elscreen-next)))))

;; Backuper les fichiers dans ~/.elisp/backups
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))

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

;; insert current time
(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%Y%m%d" (current-time))))

(global-set-key "\C-cd" 'insert-date)

;; TRAMP
(setq tramp-default-method "scpc")
;;(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>~] *")

;; On précise à M-x woman de ne pas ouvrir sa propre frame
(setq woman-use-own-frame nil)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; asciidoc mode support
(require 'asciidoc)
(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-hook 'doc-mode-hook '(lambda ()
			    (turn-on-auto-fill)
			    (require 'asciidoc)))

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

;; Magit!
;(require 'magit)
;(global-set-key (kbd "C-x g") 'magit-status)

;; egg: Emacs Got Git! (magit fork)
;;(require 'egg)
;;(setq egg-git-command/"sw/bin/git")
