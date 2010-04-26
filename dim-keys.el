;;; dim-keys.el --- some additional shortcuts
(require 'cl)

;; I can't remember having meant to use C-z as suspend-frame
(global-set-key (kbd "C-z") 'undo)

;; C-c r pour revert-buffer
(global-set-key (kbd "C-c r") '(lambda () (interactive) (revert-buffer)))

;; rcirc global shortcut to connect to servers
(global-set-key (kbd "C-c i") 'dim-rcirc-start)

;; déplacements avec Shift-<flèche>
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; déplacements d'une frame à l'autre
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-|") 'other-frame)

; winner-mode pour revenir sur le layout précédent
(winner-mode 1)

;; dired-x pour C-x C-j
(require 'dired-x)

;; dired-details pour passer à une vue courte
(require 'dired-details)
(define-key dired-mode-map "/" 'dired-details-toggle)

(require 'ibuffer)
(global-set-key "\C-x\C-b" 'ibuffer)

; jump to current buffer - © Romain Françoise
(defun ore-ibuffer-jump-to-last ()
  (ibuffer-jump-to-buffer
   (buffer-name (other-buffer (current-buffer) t))))
 (add-hook 'ibuffer-hook 'ore-ibuffer-jump-to-last)

;; use iswitchb-mode for C-x b
(iswitchb-mode)

;; C-c d pour écrire la date
(defun insert-date(&optional format)
  "Insert a time-stamp according to locale's date and time format."
  (insert (format-time-string (or format "%Y%m%d-%R") (current-time))))

(defun insert-date-blog-format() (interactive) (insert-date "%Y%m%d-%R"))
(defun insert-date-year-s-week() (interactive) (insert-date "%YS%V"))

(global-set-key (kbd "C-c d b") 'insert-date-blog-format)
(global-set-key (kbd "C-c d s") 'insert-date-year-s-week)

; find-file-at-point quand ça a du sens
(setq ffap-machine-p-known 'accept) ; no pinging
(setq ffap-url-regexp nil)          ; disable URL features in ffap
(setq ffap-ftp-regexp nil)          ; disable FTP features in ffap
(define-key global-map (kbd "C-x C-f") 'find-file-at-point)

; Hippie Expand pour un meilleur M-/ (noms de fichiers)
(require 'hippie-exp) 
(global-set-key (kbd "M-/") 'hippie-expand) 

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

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

;; C-c g pour demander à google de chercher la sélection en cours
(require 'dim-google)

;; dictionary lookups
(global-set-key (kbd "M-?") (lambda () (interactive) 
			      (dictionary-lookup-definition)))

;; déplacements sans changer la position du point dans le buffer
(global-set-key (kbd "M-<up>")
		(lambda () (interactive) (scroll-down 1) (forward-line -1)))
(global-set-key (kbd "M-S-<up>")
		(lambda () (interactive) (scroll-down 10) (forward-line -10)))

(global-set-key (kbd "M-<down>")
		(lambda () (interactive) (scroll-up 1) (forward-line 1)))
(global-set-key (kbd "M-S-<down>")
		(lambda () (interactive) (scroll-up 10) (forward-line 10)))

;; M-x svn-status
(global-set-key (kbd "C-c s") 'svn-status)

;; dict mode
(global-set-key (kbd "C-c ?") (lambda () (interactive) 
				(dictionary-lookup-definition)))

;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
	(insert current-line)
	(decf n)))))

(global-set-key (kbd "C-S-d") 'duplicate-current-line)  

;; copy-current-line
(defun copy-current-line ()
  "Copy current line to kill ring"
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(global-set-key (kbd "C-S-y") 'copy-current-line)

;; as we use C-\ for escreen we find another key for toggle-input-method,
;; which is less frequently used
(require 'dim-input-method)

(defcustom dim:my-input-method "french-dim-postfix"
  "Typing french on a qwerty keyboard... used to be latin-1-alt-postfix")

(defun dim:toggle-my-input-method ()
  "Toggle between default input method (nil) and hard coded latin-1-alt-postfix"
  (interactive) 
  (if (string= current-input-method dim:my-input-method)
      (inactivate-input-method)
    (set-input-method dim:my-input-method)))

(global-set-key (kbd "C-'") 'dim:toggle-my-input-method)
(global-set-key (kbd "C-c w") 'woman)

;; language settings, e is english, f is french
(global-set-key (kbd "C-c e") 
		(lambda ()
		  (interactive)
		  (inactivate-input-method)
		  (ispell-change-dictionary "english")))

(global-set-key (kbd "C-c f") 
		(lambda ()
		  (interactive)
		  (set-input-method dim:my-input-method)
		  (ispell-change-dictionary "francais")))

;; Diff the current buffer with the file contents
(add-to-list 'same-window-buffer-names "*Diff*")
(global-set-key (kbd "C-c =")
		(lambda () 
		  (interactive)
		  (diff-buffer-with-file (current-buffer))))

;; ELPA
(global-set-key (kbd "C-c p") 'package-list-packages)

(require 'dim-mailrc)
(require 'dim-previous-message)
(provide 'dim-keys)