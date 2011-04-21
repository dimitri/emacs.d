;;; dim-modes.el --- Dimitri Fontaine
;;

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

;; global-auto-revert-mode for automated git checkout following!
(global-auto-revert-mode 1)

(require 'w3m-load nil t)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-large-images t)
(setq mm-w3m-safe-url-regexp nil)

(require 'dim-pgsql)

(require 'woman)
(setq woman-use-own-frame nil)

;; optimisation surtout bénéfique à Tramp
(setq vc-handled-backends nil)
(setq tramp-terminal-type "screen")

;; pendant qu'on est dans Tramp, support de /sudo:remote:/path/to/file
(require 'tramp-multi-sshx)

;; M-x shell et M-x term
(require 'dim-shell-term)

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

(provide 'dim-modes)
