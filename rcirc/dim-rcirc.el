;;; dim setup for rcirc
;; http://www.emacswiki.org/emacs/rcirc

(require 'rcirc)

;; growl is for MacOSX style notification, rcirc-notify for linux libnotify
;; ones, and those are the only two systems I use...
(if (string-match "apple-darwin" system-configuration)
    (require 'growl)
  (eval-after-load 'rcirc '(require 'rcirc-notify)))

;; bitlbee: jabber & MSN from emacs.
(setq bitlbee-executable "/sw/sbin/bitlbee")
(require 'bitlbee)

;; Central place to handle connecting
(defun dim-rcirc-start ()
  "Start biltbee and rcirc, and connects to default places"
  (interactive)
  ;; under MacOSX, bitlbee has to be started here, under debian it's an
  ;; init.d daemon
  (when (string-match "apple-darwin" system-configuration)
    (bitlbee-start))
  (rcirc nil))

;; each nick its own color, notifications, extra goodies
(eval-after-load 'rcirc '(require 'rcirc-color))
(eval-after-load 'rcirc '(require 'rcirc-late-fix))

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

;; encodings
(setq rcirc-decode-coding-system 'undecided)
(setq rcirc-coding-system-alist '(("#postgresqlfr" iso-8859-15)))

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
(setq rcirc-time-format "%Y-%m-%d %H:%M ")

;; log to file please, sanely defaults to "~/.emacs.d/rcirc-log"
(setq rcirc-log-directory "~/.emacs.d/rcirc/logs")
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
	 :channels ("#emacs" "#postgresql" "#postgresqlfr" "#gli" "#cvf"))
	("irc.lost-oasis.net" 
	 :nick "dim"
	 :channels ("#vieuxcons"))
	("localhost" ("&bitlbee"))))

(provide 'dim-rcirc)