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
; load done by el-get
;(load "~/.emacs.d/nxhtml/autostart.el")
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
(global-set-key (kbd "C-x C-z") 'magit-status)

(require 'w3m-load nil t)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-large-images t)
(setq mm-w3m-safe-url-regexp nil)

(require 'dim-pgsql)

;; EMMS
(emms-standard)
;(emms-default-players) ; I want VLC mainly
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

;; pendant qu'on est dans Tramp, support de /sudo:remote:/path/to/file
(require 'tramp-multi-sshx)

;; iedit, see lib/ --- use C-: as C-; is already flyspell
(define-key global-map (kbd "C-:") 'iedit-mode)

(require 'browse-kill-ring)
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; ack is a better grep
(require 'dim-ack)

;; init org-mode
(require 'dim-org)

;; offlineimap
(require 'dim-offlineimap)

;; hl-sexp
(require 'hl-sexp)
(set-face-attribute 'hl-sexp-face nil :background "LightYellow")
;(set-face-attribute 'hl-sexp-face nil :background "RosyBrown1")
;(set-face-attribute 'hl-sexp-face nil :background "LightGoldenRod")

;; xcscope --- has been required by el-get
;; cscope debian package includes cscope-indexer, no luck under MacOSX
(setq cscope-indexing-script 
      (if (file-executable-p "/usr/bin/cscope-indexer")
	  "/usr/bin/cscope-indexer"
	"~/bin/cscope-indexer"))

(provide 'dim-modes)
