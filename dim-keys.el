;;; dim-keys.el --- some additional shortcuts

;; C-c r pour revert-buffer
(global-set-key (kbd "C-c r") '(lambda () (interactive) (revert-buffer)))

;; rcirc global shortcut to connect to servers
(global-set-key (kbd "C-c i") 'dim-rcirc-start)

;; déplacements avec Shift-<flèche>
(windmove-default-keybindings)

;; dired-x pour C-x C-j
(require 'dired-x)

;; C-c d pour écrire la date
(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%Y%m%d-%k:%M" (current-time))))

(global-set-key (kbd "C-c d") 'insert-date)

; find-file-at-point quand ça a du sens
(setq ffap-machine-p-known 'accept) ; no pinging
(setq ffap-url-regexp nil)          ; disable URL features in ffap
(setq ffap-ftp-regexp nil)          ; disable FTP features in ffap
(define-key global-map (kbd "C-x C-f") 'find-file-at-point)

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

;; déplacements sans changer la position du point dans le buffer
(global-set-key (kbd "M-<up>")   (lambda () (interactive) (scroll-down 1)) )
(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-up   1)) )

;; C-c C-t prefix numéros de tel
(defun dim:dim-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "01 73 03 42 31"))

(defun dim:fmechineau-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "01 73 03 89 43"))

(defun dim:sebl-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "02 28 08 20 71"))

(defun dim:cbouthors-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "02 28 08 20 74"))

(global-set-key (kbd "C-c ! d") 'dim:dim-numtel-pro)
(global-set-key (kbd "C-c ! f") 'dim:fmechineau-numtel-pro)
(global-set-key (kbd "C-c ! s") 'dim:sebl-numtel-pro)
(global-set-key (kbd "C-c ! c") 'dim:cbouthors-numtel-pro)

;; M-x svn-status
(global-set-key (kbd "C-c s") 'svn-status)

;; dict mode
(global-set-key (kbd "C-c ?") (lambda () (interactive) 
				(dictionary-lookup-definition)))

(provide 'dim-keys)