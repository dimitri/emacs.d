;;; dim-keys.el --- some additional shortcuts
(require 'cl)

;; C-c r pour revert-buffer
(global-set-key (kbd "C-c r") '(lambda () (interactive) (revert-buffer)))

;; rcirc global shortcut to connect to servers
(global-set-key (kbd "C-c i") 'dim-rcirc-start)

;; déplacements avec Shift-<flèche>
(windmove-default-keybindings)

;; dired-x pour C-x C-j
(require 'dired-x)

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

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; print last message
;; current-message is already lost by the time this gets called
(defun dim:previous-message (&optional nth)
  "get last line of *Message* buffer"
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (setq nth (if nth nth 1))
      (while (> nth 0)
	(previous-line)
	(setq nth (- nth 1)))
      (buffer-substring (line-beginning-position) (line-end-position)))))

(defun dim:insert-previous-message (&optional nth)
  "insert last message of *Message* to current position"
  (interactive "p")
  (insert (format "%s" (dim:previous-message nth))))

(global-set-key (kbd "C-c m") 'dim:insert-previous-message)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


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

;; on request by cyrilb, who missed it from vim
;; no global-set-key yet, still have to think I'll use it someday...
(defun copy-char-from-prev-line ()
  "Copy char at same position on previous line, when such a line and position exists"
  (interactive)
  (let ((c)
	(p (- (point) (line-beginning-position))))
    (save-excursion
      (when (eq 0 (forward-line -1))
	(when (< (+ (point) p) (line-end-position))
	  (forward-char p)
	  (setq c (thing-at-point 'char)))))
    (when c
      (insert c))))

;; escreen from http://www.splode.com/~friedman/software/emacs-lisp/
(load "escreen")
(escreen-install)

;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
	(emphased ""))

    (dolist (s escreens)
      (setq emphased
	    (concat emphased (if (= escreen-current-screen-number s)
				 (propertize (number-to-string s)
					     ;;'face 'custom-variable-tag) " ")
					     ;; 'face 'info-title-3)
					     'face 'font-lock-warning-face)
			       (number-to-string s))
		    " ")))
    (message "escreen: active screens: %s" emphased)))

(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)

(defun dim:escreen-goto-last-screen ()
  (interactive)
  (escreen-goto-last-screen)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-prev-screen (&optional n)
  (interactive "p")
  (escreen-goto-prev-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-next-screen (&optional n)
  (interactive "p")
  (escreen-goto-next-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(define-key escreen-map escreen-prefix-char 'dim:escreen-goto-last-screen)

(global-set-key (kbd "M-[") 'dim:escreen-goto-prev-screen)
(global-set-key (kbd "M-]") 'dim:escreen-goto-next-screen)
(global-set-key (kbd "C-\\ DEL") 'dim:escreen-goto-prev-screen)
(global-set-key (kbd "C-\\ SPC") 'dim:escreen-goto-next-screen)

(global-set-key '[s-mouse-4] 'dim:escreen-goto-prev-screen)
(global-set-key '[s-mouse-5] 'dim:escreen-goto-next-screen)

;; add support for C-\ from terms
(require 'term)
(define-key term-raw-map escreen-prefix-char escreen-map)
(define-key term-raw-map (kbd "M-[") 'dim:escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'dim:escreen-goto-next-screen)

;; as we use C-\ for escreen we find another key for toggle-input-method,
;; which is less frequently used
(defun dim:toggle-my-input-method ()
  "Toggle between default input method (nil) and hard coded latin-1-alt-postfix"
  (interactive) 
  (if (string= current-input-method "latin-1-alt-postfix")
      (inactivate-input-method)
    (set-input-method "latin-1-alt-postfix")))

(if (string-match "apple-darwin" system-configuration)
    (global-set-key (kbd "M-§") 'dim:toggle-my-input-method)
  (global-set-key (kbd "s-i") 'dim:toggle-my-input-method))

;; mails
;(global-set-key (kbd "C-c @") 'mail-abbrev-insert-alias)
(require 'message)
(define-key message-mode-map (kbd "C-'") 'mail-abbrev-complete-alias)

;; automate adding mail at point to ~/.mailrc
(require 'mail-parse)

(defun thing-at-point-bounds-of-email-address ()
  "return a cons of begin and end position of email address at point, including full name"
  (save-excursion
    (let* ((search-point (point))
	   (start (re-search-backward "[:,]" (line-beginning-position) 'move))
	   (dummy (goto-char search-point))
	   (end   (re-search-forward  "[:,]" (line-end-position) t)))
      (setq start (if start (+ 1 start)
		    (line-beginning-position)))
      (unless end (setq end (line-end-position)))
      (cons start end))))

(defun thing-at-point-email-address ()
  "return full email address at point"
  (let* ((bounds (thing-at-point-bounds-of-email-address))
	 (email-address-text
	  (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (mail-header-parse-address email-address-text)))

(put 'email-address 'bounds-of-thing-at-point 'thing-at-point-bounds-of-email-address)
(put 'email-address 'thing-at-point 'thing-at-point-email-address)

(defun dim:mailrc-add-entry (&optional prefix alias)
  "read email at point and add it to an ~/.mailrc file"
  (interactive "P\nMalias: ")
  (let* ((default-mailrc (file-name-nondirectory mail-personal-alias-file))
	 (mailrc (if prefix (expand-file-name
			     (read-file-name 
			      "Add alias into file: " 
			      "~/" 
			      default-mailrc
			      t
			      default-mailrc))
		   mail-personal-alias-file))
	 (address (thing-at-point 'email-address))
	 (buffer (find-file-noselect mailrc t)))
    (when address
      (with-current-buffer buffer
	;; we don't support updating existing alias in the file
	(save-excursion
	  (goto-char (point-min))
	  (if (search-forward (concat "alias " alias) nil t)
	      (error "Alias %s is already present in .mailrc" alias)))

	(save-current-buffer
	  (save-excursion
	    (goto-char (point-max))
	    (insert (format "\nalias %s \"%s <%s>\"" alias (cdr address) (car address)))))))))

(global-set-key (kbd "C-c C-@") 'dim:mailrc-add-entry)

(provide 'dim-keys)