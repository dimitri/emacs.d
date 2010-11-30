;;; dim-others.el --- Dimitri Fontaine
;; no provide, not required, random functions and pastes

(defun twinkle-call-symbol-or-region ()
  "Call the phone number at point (symbol seems good enough), or in region"
  (interactive)
  (shell-command-to-string 
   (format "twinkle --cmd 'call %s'"
	   (replace-regexp-in-string 
	    "[^0-9+]" "" 
	    (if (use-region-p)
		(buffer-substring (region-beginning) (region-end))
	      (thing-at-point 'symbol))))))

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

;;;
;;; jd.gnus.buttons.el --- Dimitri Fontaine & Julien Danjou
;;
;; Buttonize things

(defvar jd:orig-gnus-button-alist gnus-button-alist
  "Allow to reset the button list to the default gnus one when changing group")

(defun jd:gnus-button-browse (url)
  "produce a function that browse an url formated to contain the number"
  `(lambda (n) (browse-url (format ,url n))))

(defcustom jd:gnus-buttons-alist 
  '(("debian" 
     "\\#\\([0-9]+\\)" 
     "http://bugs.debian.org/%s")

    ("Easter-eggs"
     "\\#\\([0-9]+\\)" 
     "https://ssl.easter-eggs.fr/rt/Ticket/Display.html?id=%s"))
  "Setup for generating buttons and adding them to gnus when selecting a group")

(defun jd:gnus-add-buttons (regexp func)
  "add buttons for given regexp and function"
  (add-to-list 
   'gnus-header-button-alist (list "Subject:" regexp 0 t func 1))
  (add-to-list 
   'gnus-button-alist (list regexp 0 t func 1)))

(defun jd:gnus-buttonize ()
  "Check for buttons setup to add to current newsgroup"
  (setq gnus-button-alist jd:orig-gnus-button-alist)
  (mapc
   (lambda (x)
     (when (string-match (car x) gnus-newsgroup-name)
       (let ((regexp (cadr x))
	     (url    (caddr x)))
	 (jd:gnus-add-buttons regexp (jd:gnus-button-browse url)))))
   jd:gnus-buttons-alist))

;(add-hook 'gnus-select-group-hook 'jd:gnus-buttonize)

;;;
;; from rgr on #emacs, reopen current file as root
;; bind on C-x C-r e.g.

(defun find-alternative-file-with-sudo ()
  "Open current buffer as root!"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))

;;
;; renard activates the remote directory tracking in M-x term thusly:
;;
(defun cw:term:run ()
  "Run an ansi-terminal."
  (interactive)
  (let* ((current-buffer-file (buffer-file-name))
	 (current-dir
	  ;; only if buffer is a dired.
	  (if (string-match (symbol-name major-mode) "dired-mode")
	      (dired-current-directory)
	    ;; Else find file directory from buffer name.
	    (when current-buffer-file
	      (file-name-directory current-buffer-file))))
	 (current-host))
    (when current-dir
      (save-match-data
	(when (string-match "^/scp:\\([^:]+\\):\\(.*\\)" current-dir)
	  (setq current-host (match-string 1 current-dir))
	  (setq current-dir (match-string 2 current-dir)))))
    ;; Run terminal
    (if current-host
	;; run remote shell
	(progn
	  (cssh-term-create current-host)
	  (term-send-input)
	  ;; Make sure Bash is the default shell
	  (insert " HISTCONTROL=ignoreboth exec bash")
	  (term-send-input)
	  ;; Launch remote directory tracking
	  (insert
	   (concat
	    " function prompt_cmd { "
	    "echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)};"
	    "echo -e \"\\033AnSiTc\" $(pwd);"
	    "echo -e \"\\033AnSiTh\" ${TRAMP_HOSTNAME-$(hostname)}; };"
	    " export PROMPT_COMMAND=prompt_cmd ; unset HISTCONTROL"))
	  (term-send-input))
      ;; run local shell
      (ansi-term "/bin/bash"))
    ;; change directory if needed.
    (when current-dir
      (insert (concat " cd " current-dir))
      (term-send-input))))
