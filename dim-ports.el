;;; dim-ports.el --- Dimitri Fontaine
;;
;; Some portability oriented stuff, macros

;; ok as of now it's the same on all system, but still, it has its place here.
(defun get-domain-name (&optional from)
  "Returns the system domain name.  If FROM is 'resolv or nil,
returns the value defined in /etc/resolv.conf."
  (when (or (null from) (eq from 'resolv))
    (when (file-readable-p "/etc/resolv.conf")
      (with-temp-buffer
	(insert-file-contents-literally "/etc/resolv.conf")
	(when (re-search-forward "^domain \\([^ ]+\\)$" nil t)
	  (match-string 1))))))

(defun lsb-release (&optional property)
  "Parse lsb-release output and return an alist, or the value for the given property"
  (when (file-executable-p "/usr/bin/lsb_release")
    (let* ((lsbr (shell-command-to-string "/usr/bin/lsb_release -a 2>/dev/null"))
	   (props (split-string lsbr "[:\n]" t))
	   (kv))
      (while (>= (length props) 2)
	;; Don't keep extra spaces. This way seems like the easy one in elisp.
	(let ((key (mapconcat 'identity (split-string (car props)) " "))
	      (val (mapconcat 'identity (split-string (cadr props)) " ")))
	  (setq kv (add-to-list 'kv (cons key val)))
	  (setq props (cddr props))))
      (if property
	  (cdr (assoc property (lsb-release)))
	kv))))

;; my try at walk-path
(defun walk-path (path fun &optional match-regexp depth-first)
  "walk given path recursively, calling fun for each entry"
  (dolist (e (directory-files-and-attributes path t match-regexp))
      (let* ((filename   (car e))
	     (attributes (cdr e))
	     (is-subdir  (nth 0 attributes)))
	;; skip . and ..
	(unless (string-match "/\\.\\.?$" filename)
	  (if (and is-subdir depth-first)
	      (progn
		(walk-path filename fun match-regexp depth-first)
		(funcall fun filename attributes))

	    (funcall fun filename attributes)
	    (when is-subdir
	      (walk-path filename fun match-regexp depth-first)))))))

(defun walk-path-list (path &optional match-regexp depth-first)
  "walk given path and build a list of (filename . attributes)"
  (let ((l))
    (walk-path path #'(lambda (f a) (add-to-list 'l (cons f a))) 
	       match-regexp depth-first) l))
      
;; thanks to ams on #emacs on irc.freenode.net
(defmacro with-window-system (&rest body) 
  "eval body only when running an windowed version of Emacs"
  `(when window-system ,@body))

(defmacro without-window-system (&rest body) 
  "eval body only when running a console Emacs"
  `(unless window-system ,@body))

;; variations on the theme
(defmacro when-running-debian (&rest body)
  "eval body only when running under debian"
  ;; FIXME: check "lsb_release -a" output on debian/kFreeBSD
  `(when (equal (lsb-release "Distributor ID") "Debian") ,@body))

(defmacro when-running-ubuntu (&rest body)
  "eval body only when running under debian"
  ;; FIXME: check "lsb_release -a" output on debian/kFreeBSD
  `(when (equal (lsb-release "Distributor ID") "Ubuntu") ,@body))

(defmacro when-running-debian-or-ubuntu (&rest body)
  "eval body only when running under debian"
  ;; FIXME: check "lsb_release -a" output on debian/kFreeBSD
  `(when  (or (equal (lsb-release "Distributor ID") "Debian")
	      (equal (lsb-release "Distributor ID") "Ubuntu")) ,@body))

(defmacro when-running-macosx (&rest body)
  "eval body only when running under MacOSX"
   `(when (string-match "apple-darwin" system-configuration) ,@body))

;; Used from .gnus msmtp
(defmacro when-using-msmtp (&rest body)
  "eval body only when ~/.msmtprc exists and is readable"
  `(when (file-readable-p (expand-file-name "~/.msmtprc")) ,@body))

(provide 'dim-ports)
