;;; dim-ports.el --- Dimitri Fontaine
;;
;; Some portability oriented stuff, macros

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
