;;; dim setup for rcirc
;; http://www.emacswiki.org/emacs/rcirc

(require 'rcirc)
(require 'rcirc-groups)
;(require 'rcirc-notify-mode)


;; Some utility functions to have automated layout upon startup
(require 'cl)

(defun dim:wait-for-buffers (buffer-list &optional n)
  "return only when all buffers in the list exist, or we tried n times"
  (let ((n (or n 2))
	(buffers))
    (while (and (< (length buffers) (length buffer-list) (not (eq n 0)))
      (dolist (buffer-name buffer-list)
	(when (not (memq buffer-name buffers))
	  (when (get-buffer buffer-name)
	    (setq buffers (cons buffer-name buffers)))))
      (message "%s, waiting for %d buffers still: %S" 
	       n (- (length buffer-list) (length buffers)) buffers)
      (sleep-for 0.5)
      (decf n))
    ;; we return buffers
    buffers))

(defun dim:rcirc-layout-home ()
  "Organise screen layout for IRC setup"
  (let ((buffers  (dim:wait-for-buffers '("#vieuxcons@irc.lost-oasis.net"
					  "#emacs@irc.freenode.net"
					  "#postgresql@irc.freenode.net"))))
    (when (eq 3 (length buffers))
      (delete-other-windows)
      (let ((right-window (split-window-horizontally)))
	(set-window-buffer (selected-window) "#vieuxcons@irc.lost-oasis.net")
	(set-window-buffer right-window "#postgresql@irc.freenode.net")
	(select-window (split-window-vertically))
	(set-window-buffer (selected-window) "#emacs@irc.freenode.net")))))

(defun dim:rcirc-layout-work ()
  "Organise screen layout for IRC setup"
  (let ((buffers (dim:wait-for-buffers '("#vieuxcons@irc.lost-oasis.net"
					 "#pg@irc.hi-media-techno.com"
					 "#postgresql@irc.freenode.net"
					 "#emacs@irc.freenode.net"))))
    (when (eq 4 (length buffers))
      (delete-other-windows)
      (let* ((right-window (split-window-horizontally))
	     (bottom-window (split-window-vertically)))
	(set-window-buffer (selected-window) "#postgresql@irc.freenode.net")
	(set-window-buffer bottom-window "#vieuxcons@irc.lost-oasis.net")
	(select-window bottom-window)
	(set-window-buffer (split-window-vertically) "#emacs@irc.freenode.net")
	(set-window-buffer right-window "#pg@irc.hi-media-techno.com")
	(balance-windows)
	;; now cut the right part
	(select-window right-window)
	(split-window-vertically-quarter-bottom)
	(rcirc-groups:switch-to-groups-buffer)))))

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

  ;; now start rcirc
  (rcirc nil)

  ;; and organize a nice layout
  (if (string-match "apple-darwin" system-configuration)
      (dim:rcirc-layout-home)
    (dim:rcirc-layout-work)))


;; each nick its own color, notifications, extra goodies
(eval-after-load 'rcirc '(require 'rcirc-color))
(eval-after-load 'rcirc '(require 'rcirc-late-fix))

;; growl is for MacOSX style notification, rcirc-notify for linux libnotify
;; ones, and those are the only two systems I use...
(if (string-match "apple-darwin" system-configuration)
  (require 'growl)

  ;; rcirc-notify for having system tray style pop-ups, -mode for having emacs
  ;; like support for notifications in *rcirc-notify* buffer (special mode)
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

;; whois on private even if I'm receiving it
;; (add-hook 'rcirc-mode-hook 
;; 	  (lambda () 
;; 	    (when (and (not (rcirc-channel-p rcirc-target))
;; 		       (not (string= sender (rcirc-nick proc))))
;; 	      (rcirc-cmd-whois (rcirc-nick rcirc-process)))))

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
(setq rcirc-authinfo '(("freenode" nickserv "dim" "ernieball")
		       ("lost-oasis" nickserv "dim" "ernieball")
		       ("localhost" bitlbee "dim" "secret")))

(setq rcirc-server-alist
      '(("irc.freenode.net" 
	 :channels ("#postgresql" "#skytools" "#postgresqlfr"
		    "#emacs" "#gli" "#cvf"))
	("irc.lost-oasis.net" 
	 :nick "dim"
	 :channels ("#vieuxcons"))
	("localhost" ("&bitlbee"))))

(when (string-match "hi-media-techno" (get-domain-name))
  (add-to-list 'rcirc-server-alist
	       '("irc.hi-media-techno.com"
		 :channels 
		 ("#hm" "#pg" "#eurovox" "#allopass" "#comtrack" "#admin"))))

(when (not (string-match "hi-media-techno" (get-domain-name)))
  ;; I want to define it but I'd rather not connect to it by default
  (add-to-list 'rcirc-server-alist
	       '("irc.free.fr"
		 :nick "bob`"
		 :user-name "bob"
		 :full-name "bob")))

(provide 'dim-rcirc)
