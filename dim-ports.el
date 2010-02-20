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
  ;; beware of debian/kFreeBSD. Yes I intend to be using it.
  `(when (string-match "Debian" (emacs-version)) ,@body))

(defmacro when-running-macosx (&rest body)
  "eval body only when running under MacOSX"
   `(when (string-match "apple-darwin" system-configuration) ,@body))

;; Used from .gnus msmtp
(defmacro when-using-msmtp (&rest body)
  "eval body only when ~/.msmtprc exists and is readable"
  `(when (file-readable-p (expand-file-name "~/.msmtprc")) ,@body))

(provide 'dim-ports)
