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

;; M-x re-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; optimisation surtout bénéfique à Tramp
(setq vc-handled-backends nil)
(setq tramp-terminal-type "screen")

;; /sudo:remote:/path/to/file
;; (set-default 'tramp-default-proxies-alist
;; 	     (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; pendant qu'on est dans Tramp, support de /sudo:remote:/path/to/file
(require 'tramp-multi-sshx)
(require 'cw-tramp)

;; don't unneeded keep stuff around
(defadvice tramp-open-connection-setup-interactive-shell
  (before cw:tramp-open-connection-setup-interactive-shell activate)
  "Add process-sentinel to tramp-shells. Kill buffer when process died."
  (set-process-sentinel
   ;; Arg 0 is proc
   (ad-get-arg 0)
   (lambda (proc change)
     (when (eq (process-status proc) 'exit)
       (kill-buffer (process-buffer proc))))))

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

;; C-c C-l in php buffers to lint the current buffer
(require 'dim-php)

(provide 'dim-modes)
