;;; dim-modes.el --- Dimitri Fontaine
;;

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(require 'asciidoc)
(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-hook 'doc-mode-hook '(lambda ()
			    (turn-on-auto-fill)
			    (require 'asciidoc)))

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

;; nxhtml
(load "~/.emacs.d/nxhtml/autostart.el")
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
				    (background dark)) nil))))


(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'w3m-load nil t)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-large-images t)
(setq mm-w3m-safe-url-regexp nil)

(require 'dim-pgsql)

;; EMMS
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; Show the current track each time EMMS
;; starts to play a track with "NP : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "EMMS Now Playing: %s")

(add-hook 'dired-load-hook
	  (define-key dired-mode-map (kbd "E") 'emms-play-dired))

(require 'woman)
(setq woman-use-own-frame nil)

;; M-x term
(add-hook 'term-mode-hook 
	  (lambda () (setq truncate-lines t)))

(add-hook 'term-mode-hook 
	  (lambda () (hl-line-mode -1)))

;; dict
(require 'dictionary)
(setq dictionary-coding-systems-for-dictionaries '(("robert" . iso-8859-15)))
(unless (string-match "apple-darwin" system-configuration)
  (setq dictionary-server "localhost"))

;; optimisation surtout bénéfique à Tramp
(setq vc-handled-backends nil)

;; iedit, see lib/ --- use C-: as C-; is already flyspell
(define-key global-map (kbd "C-:") 'iedit-mode)

;; ack is a better grep
(require 'dim-ack)

;; offlineimap
(require 'dim-offlineimap)
(provide 'dim-modes)
