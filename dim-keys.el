;;; dim-keys.el --- some additional shortcuts
(require 'cl)

;; I can't remember having meant to use C-z as suspend-frame
(global-set-key (kbd "C-z") 'undo)

;; M-z is zap-to-char, arrange it though
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR."
    (insert char)
    (forward-char -1))

;; C-M-z is an experimentation from vimpulse, go to given char
(defun dim:forward-to-char (char)
  "Go to next given char"
  (interactive "cForward to char: ")
  (if (not (eq 'dired-mode major-mode))
      (search-forward (char-to-string char))
    ;; in dired-mode, only match file names
    (goto-char
     (let ((p (point)))
       (goto-char (point-min))
       (catch 'found
	 (while (not (eobp))
	   (let ((n (dired-get-filename 'verbatim t)))
	     (when (and n (string= (substring n 0 1) (char-to-string char)))
	       (throw 'found (point))))
	   (dired-next-line 1))
	 p)))))

(global-set-key (kbd "C-M-z") 'dim:forward-to-char)

;; rebind C-o and M-o to more useful variants of open-line
(require 'dim-open-line)

;; C-c r pour revert-buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

;; rcirc global shortcut to connect to servers
(global-set-key (kbd "C-c i") 'dim-rcirc-start)

;; déplacements avec M-<flèche>, sauf dans la VM VirtualBox
(if (string-match "darkstar" system-name)
    (windmove-default-keybindings 'super)
  (windmove-default-keybindings 'meta))
(setq windmove-wrap-around t)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; déplacements d'une frame à l'autre
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-|") 'other-frame)
(global-set-key (kbd "C-x \\") 'other-frame)
(global-set-key (kbd "C-x C-\\") 'other-frame)

; winner-mode pour revenir sur le layout précédent
(winner-mode 1)

;; Quelques outils locaux
(require 'dim-splits)

;; C-x 2 et C-x 3 et C-x 0 ne sont pas si pratiques
(global-set-key (kbd "C-x C-;") 'delete-window)
(global-set-key (kbd "C-x C-'") 'delete-other-windows)
(global-set-key (kbd "C-x C-,") 'split-window-vertically)
(global-set-key (kbd "C-x C-.") 'split-window-horizontally)

;; Et du coup on adapte C-c 2 sur le même modèle
(global-set-key (kbd "C-c C-,") 'split-window-vertically-quarter-bottom)

(defun dim:dired-default-directory ()
  "open dired in the current default directory"
  (interactive)
  (dired default-directory))

;; dired-x pour C-x C-j
(require 'dired-x)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
(define-key dired-mode-map (kbd "<backspace>") 'dired-kill-subdir)
(define-key dired-mode-map (kbd "TAB") 'dired-hide-subdir)
(global-set-key (kbd "C-x C-d") 'dim:dired-default-directory)

;; dired-details pour passer à une vue courte
(require 'dired-details)
(define-key dired-mode-map "/" 'dired-details-toggle)

;; open files with the MacOSX default application
(defun dired-do-shell-mac-open ()
  (interactive)
  (save-window-excursion
    (dired-do-async-shell-command
     "open" current-prefix-arg
     (dired-get-marked-files t current-prefix-arg))))

(define-key dired-mode-map (kbd "C-o") 'dired-do-shell-mac-open)

(require 'ibuffer)
(global-set-key "\C-x\C-b" 'ibuffer)

; jump to current buffer - © Romain Françoise
(defun ore-ibuffer-jump-to-last ()
  (ibuffer-jump-to-buffer
   (buffer-name (other-buffer (current-buffer) t))))
 (add-hook 'ibuffer-hook 'ore-ibuffer-jump-to-last)

;; find-file-at-point, deprecated, see ido
;; (setq ffap-machine-p-known 'accept) ; no pinging
;; (setq ffap-url-regexp nil)          ; disable URL features in ffap
;; (setq ffap-ftp-regexp nil)          ; disable FTP features in ffap
;; (define-key global-map (kbd "C-x C-f") 'find-file-at-point)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)

;; invert C-x b and C-x C-b, the all control one is easier to type
(define-key global-map (kbd "C-x C-b") 'ido-switch-buffer)
(define-key global-map (kbd "C-x C-c") 'ido-switch-buffer) ; use M-x kill-emacs
(define-key global-map (kbd "C-x b") 'ido-switch-buffer)
(define-key global-map (kbd "C-x B") 'ibuffer)

(defun dim:kill-buffer-name (arg)
  "Kill the current buffer's filename and show it in the echo area."
  (interactive "P")
  (let ((bfn (if arg (buffer-name) (or buffer-file-name (buffer-name)))))
    (kill-new bfn)
    (message "%s" bfn)))

(global-set-key (kbd "C-c n") 'dim:kill-buffer-name)

;; user defined completing-read-function entered in emacs24
;; restore with: (setq completing-read-function 'completing-read-default)
(when (boundp 'completing-read-function)
  (defun ido-completing-read* (prompt choices &optional predicate require-match
				      initial-input hist def inherit-input-method)
    "Adjust arguments when it's necessary"
    (if (and (listp choices) (not (functionp choices)))
	(ido-completing-read
	 prompt
	 (mapcar (lambda (c) (if (listp c) (car c) c)) choices)
	 predicate require-match initial-input hist def inherit-input-method)
      (completing-read-default prompt choices predicate require-match
			       initial-input hist def inherit-input-method)))

  (setq completing-read-function 'ido-completing-read*))

;; C-c d pour écrire la date
(defun insert-date(&optional format)
  "Insert a time-stamp according to locale's date and time format."
  (insert (format-time-string (or format "%Y%m%d-%R") (current-time))))

(defun insert-date-blog-format() (interactive) (insert-date "%Y%m%d-%R"))
(defun insert-date-year-s-week() (interactive) (insert-date "%YS%V"))

(global-set-key (kbd "C-c d b") 'insert-date-blog-format)
(global-set-key (kbd "C-c d s") 'insert-date-year-s-week)

; Hippie Expand pour un meilleur M-/ (noms de fichiers)
(require 'hippie-exp)
(global-set-key (kbd "M-/") 'hippie-expand)

;; broken in current snapshot
;; GNU Emacs 24.0.50.1 (x86_64-pc-linux-gnu, GTK+ Version 2.24.4) of 2011-07-05
(when-running-debian-or-ubuntu
 (global-set-key (kbd "M-/") 'dabbrev-expand))

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
(global-set-key (kbd "M-?") 'dictionary-lookup-definition)

;; déplacements sans changer la position du point dans le buffer
(global-set-key (kbd "M-S-<up>")
		(lambda () (interactive) (scroll-down 1) (forward-line -1)))

(global-set-key (kbd "M-S-<down>")
		(lambda () (interactive) (scroll-up 1) (forward-line 1)))

;; M-x svn-status
(global-set-key (kbd "C-c s") 'svn-status)

;; dict mode
(global-set-key (kbd "C-c ?") (lambda () (interactive)
				(dictionary-lookup-definition)))

;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (let ((nb (or n 1))
	(current-line (thing-at-point 'line)))
    (save-excursion
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
	(insert current-line)
	(decf n)))
    ;; now move down as many lines as we inserted
    (next-line nb)))

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

;; Global key for input method, but we steal it in terms
(global-set-key (kbd "C-'") 'dim:toggle-my-input-method)

;; woman
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

;; IELM
(global-set-key (kbd "C-M-;") 'ielm)

;; resolve name/ip at point and place the result in the kill ring
(require 'net-utils)
(defun dim:dns-lookup-host ()
  "Lookup the DNS information for HOST at point (name or IP address)."
  (interactive)
  (let* ((host (net-utils-machine-at-point))
	 (default-directory "/tmp")	; LOCAL resolver please
	 (lines (process-lines dns-lookup-program host))
	 (first (car lines))
	 (split (split-string first)))
    (message "%s" first)
    ;; ("google.fr" "has" "address" "74.125.230.84")
    ;; ("xxxx.in-addr.arpa" "domain" "name" "pointer" "name.domain.tld.")
    (cond ((and (eq 4 (length split))
		(equal (nth 1 split) "has")
		(equal (nth 2 split) "address"))
	   (kill-new (nth 3 split)))

	  ((and (eq 5 (length split))
		(equal (nth 1 split) "domain")
		(equal (nth 2 split) "name")
		(equal (nth 3 split) "pointer"))
	   (kill-new (nth 4 split))))))

(global-set-key (kbd "C-M-S-H") 'dim:dns-lookup-host)

;; M-x shell on a remote host
(require 'cssh)
(defun dim:remote-shell (hostname)
  "Open a M-x shell buffer connected to a remote ssh account"
  (interactive
   (list (completing-read "Remote host: " (cssh-get-hosts-list))))
  (let ((default-directory (format "/%s:%s" hostname "~"))
	(bufname           (format "*sshell %s*" hostname)))
    (shell bufname)))

(global-set-key (kbd "C-+") 'dim:remote-shell)

;; Increment number at point
(defun dim:increment-number-at-point (&optional prefix)
  (interactive "p")
  (let* ((beg    (skip-chars-backward "0-9a-fA-F"))
	 (hexa   (save-excursion (forward-char -2) (looking-at-p "0x")))
	 ;; force the prefix to hexa (4) we see "0x" before the number
	 (prefix (if hexa 4 prefix))
	 (end    (re-search-forward "[0-9a-fA-F]+" nil t))
	 (nstr   (match-string 0))
	 (l      (- (match-end 0) (match-beginning 0)))
	 (fmt    (format "%%0%d" l)))
    (message "PLOP: %d" prefix)
    (destructuring-bind (base format)
	(case prefix
	  ((1)  '(10 "d"))		; no command prefix, decimal
	  ((4)  '(16 "x"))		; C-u, hexadecimal
	  ((16) '(8 "o")))		; C-u C-u, octal
      (let* ((n   (string-to-number nstr base))
	     (n+1 (+ n 1))
	     (fmt (format "%s%s" fmt format)))
	(replace-match (format fmt n+1))))))

(global-set-key (kbd "C-c +") 'dim:increment-number-at-point)

;; Create a new buffer, not associated to a file, and switch to it
(defun dim:make-buffer (&optional buffer-name)
  (interactive "BSwitch (or create) to buffer: ")
  (switch-to-buffer (get-buffer-create buffer-name)))

(global-set-key (kbd "C-c C-f") 'dim:make-buffer)

(require 'dim-mailrc)
(require 'dim-previous-message)
(provide 'dim-keys)
