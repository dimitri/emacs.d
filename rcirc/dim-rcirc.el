;;; dim setup for rcirc
;; http://www.emacswiki.org/emacs/rcirc

(require 'rcirc)

;; Central place to handle connecting
(defun dim-rcirc-start ()
  "Start biltbee and rcirc, and connects to default places"
  (interactive)

  ;; under MacOSX, bitlbee has to be started here, under debian it's an
  ;; init.d daemon
  (when (string-match "apple-darwin" system-configuration)
    (require 'bitlbee)
    (setq bitlbee-executable "/sw/sbin/bitlbee")
    (bitlbee-start))
  (rcirc nil))

;; each nick its own color, notifications, extra goodies
(eval-after-load 'rcirc '(require 'rcirc-color))
(eval-after-load 'rcirc '(require 'rcirc-late-fix))

;; growl is for MacOSX style notification, rcirc-notify for linux libnotify
;; ones, and those are the only two systems I use...
(if (string-match "apple-darwin" system-configuration)
    (require 'growl)
  (eval-after-load 'rcirc '(require 'rcirc-notify)))

;; /reconnect
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
	    (port (process-contact process :service))
	    (nick (rcirc-nick process))
	    channels query-buffers)
       (dolist (buf (buffer-list))
	 (with-current-buffer buf
	   (when (eq process (rcirc-buffer-process))
	     (remove-hook 'change-major-mode-hook
			  'rcirc-change-major-mode-hook)
	     (if (rcirc-channel-p rcirc-target)
		 (setq channels (cons rcirc-target channels))
	       (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
		      rcirc-default-user-name
		      rcirc-default-user-full-name
		      channels))))

(defvar dim:dynamic-fill-column-margin 3
  "Safety margin used to calculate fill-column depending on window-width")

;; dynamically set fill-column at redisplay time
(defun dim:dynamic-fill-column-window (window &optional margin)
  "Dynamically get window's width and adjust fill-column accordingly"
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'rcirc-mode)
      (setq fill-column
	    (- (window-width window) 
	       (or margin dim:dynamic-fill-column-margin))))))

(defun dim:dynamic-fill-column (frame)
  "Dynamically tune fill-column for a frame's windows at redisplay time"
  (walk-windows 'dim:dynamic-fill-column-window 'no-minibuf frame))
  
(eval-after-load 'rcirc
  '(add-to-list 'window-size-change-functions 'dim:dynamic-fill-column))

;; encodings
(setq rcirc-decode-coding-system 'undecided)
;(setq rcirc-coding-system-alist '(("#postgresqlfr" . iso-8859-15)))

(eval-after-load 'rcirc
  '(defun-rcirc-command encoding (arg)
     "Change the encoding coding system
`rcirc-encode-coding-system' for the current buffer only."
     (interactive)
     (if (string= arg "")
	 (rcirc-print process (rcirc-nick process) "NOTICE" target
		      (symbol-name rcirc-encode-coding-system))
       (setq rcirc-encode-coding-system (intern-soft arg)))))

;; timestamps formating
(require 'dim-rcirc-log)
(setq rcirc-time-format "%H:%M ")
(setq rcirc-log-time-format "%Y-%m-%d %H:%M ")

;; log to file please, sanely defaults to "~/.emacs.d/rcirc-log"
(setq rcirc-log-directory "~/.elisp/rcirc/logs")
(setq rcirc-log-flag t)

;; Keep input line at bottom.
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (set (make-local-variable 'scroll-conservatively)
		 8192)))

;; User defaults
(setq rcirc-default-nick "dim")
(setq rcirc-default-user-name "Dimitri")
(setq rcirc-default-user-full-name "Dimitri Fontaine")

;; p4ssw0rd
(setq rcirc-authinfo '(("freenode" nickserv "dim" "PHOQUE")
		       ("lost-oasis" nickserv "dim" "PHOQUE")))

(setq rcirc-server-alist
      '(("irc.freenode.net" 
	 :channels ("#postgresql" "#skytools" "#postgresqlfr"
		    "#emacs" "#gli" "#cvf"))
	("irc.lost-oasis.net" 
	 :nick "dim"
	 :channels ("#vieuxcons"))
	("localhost" ("&bitlbee"))
	("irc.hi-media-techno.com"
	 :channels ("#hm" "#pg" "#eurovox" "#allopass" "#comtrack" "#admin"))))

(provide 'dim-rcirc)
