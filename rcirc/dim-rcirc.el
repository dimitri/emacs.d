;;; dim setup for rcirc
;; http://www.emacswiki.org/emacs/rcirc

(require 'rcirc)
(require 'rcirc-groups)
;(require 'rcirc-notify-mode)

;; Exclude rcirc properties when yanking, in order to be able to send mails
;; for example.
(add-to-list 'yank-excluded-properties 'rcirc-text)

;; Some utility functions to have automated layout upon startup
(require 'cl)

(defun dim:wait-for-buffers (buffer-list &optional n)
  "return only when all buffers in the list exist, or we tried n times"
  (let ((n (or n 2))
	(buffers))
    (while (and (< (length buffers) (length buffer-list)) (not (eq n 0)))
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

(defun dim:rcirc-layout-home-waiting ()
  "Organize screen layout for seeing the IRC connections"
  (delete-other-windows)
  (set-window-buffer (selected-window) "*tapoueh.org*")
  (set-window-buffer (split-window-horizontally) "*pgsql.tapoueh.org*"))

(defun dim:rcirc-layout-home ()
  "Organise screen layout for IRC setup"
  (let ((buffers  (dim:wait-for-buffers '("#vieuxcons@tapoueh.org"
					  "#emacs@pgsql.tapoueh.org"
					  "#postgresql@pgsql.tapoueh.org"))))
    (if (not (eq 3 (length buffers)))
	(dim:rcirc-layout-home-waiting)
      (delete-other-windows)
      (let ((upper-left-window (selected-window))
	    (right-window (split-window-horizontally)))
	(set-window-buffer (selected-window) "#vieuxcons@tapoueh.org")
	(set-window-buffer right-window "#postgresql@pgsql.tapoueh.org")
	(select-window (split-window-vertically))
	(set-window-buffer (selected-window) "#emacs@pgsql.tapoueh.org")
	(select-window upper-left-window)
	(split-window-vertically-quarter-bottom)
	(rcirc-groups:switch-to-groups-buffer)))))


(defun dim:rcirc-layout-work ()
  "Organise screen layout for IRC setup"
  (let ((buffers (dim:wait-for-buffers '("#vieuxcons@tapoueh.org"
					 "#dba@irc.hi-media-techno.com"
					 "#postgresql@pgsql.tapoueh.org"
					 "#emacs@pgsql.tapoueh.org"))))
    (if (not (eq 4 (length buffers)))
	;; yes, at work too, same servers
	(dim:rcirc-layout-home-waiting)
      (delete-other-windows)
      (let* ((right-window (split-window-horizontally))
	     (bottom-window (split-window-vertically)))
	(set-window-buffer (selected-window) "#postgresql@pgsql.tapoueh.org")
	(set-window-buffer bottom-window "#vieuxcons@tapoueh.org")
	(select-window bottom-window)
	(set-window-buffer (split-window-vertically) "#emacs@pgsql.tapoueh.org")
	(set-window-buffer right-window "#dba@irc.hi-media-techno.com")
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
(defadvice rcirc-handler-PRIVMSG
  (before dim:rcirc-handler-PRIVMSG activate)
  (unless (or (rcirc-channel-p (car args))
	      (and (boundp 'dim:rcirc-handler-whois) dim:rcirc-handler-whois))
    (message "PLOP")
    (with-current-buffer (rcirc-get-buffer process sender t)
      (rcirc-cmd-whois sender process)
      (make-local-variable 'dim:rcirc-handler-whois)
      (setq dim:rcirc-handler-whois t))))

(defun dim:rcirc-whois-on-query-from-others ()
  (when (and target (not (rcirc-channel-p target)))
    ;; as this buffer ain't ready to receive the answer, it'll
    ;; go into the process server buffer
    ;;(rcirc-cmd-whois target (get-buffer-process (current-buffer)))
    ;; (rcirc-cmd-whois target (rcirc-get-buffer process target t))
    (rcirc-cmd-whois target process)
    (let (whois)
      (with-rcirc-process-buffer process
	(save-excursion
	  (forward-line -1)
	  (let ((p (point))
		(b (re-search-backward
		    (concat "^.*\*\*\* 311 " target) nil t)))
	    (when b
	      (setq whois (buffer-substring b p))))))
      ;; insert the whois into the new buffer now, at the very
      ;; beginning of it
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char rcirc-prompt-start-marker)
	  (if whois
	      (insert-before-markers whois)
	    (insert-before-markers "didn't get whois back on-time"
				   (format " (%S %S)" target process)
				   "\n")))))))

(setq rcirc-mode-hook nil)
(add-hook 'rcirc-mode-hook 'dim:rcirc-whois-on-query-from-others)

;; avoid ido-completing-read* sometimes (problem with history)
(defadvice rcirc-browse-url
  (around dim:rcirc-browse-url activate)
  "Avoid using ido-completing-read* in rcirc-browse-url"
  (let ((completing-read-function 'completing-read-default))
    ad-do-it))

;; indicate-buffer-boundaries in rcirc is not doing the right thing
(defun dim:rcirc-set-indicate-buffer-boundaries ()
  (setq indicate-buffer-boundaries '((top . left) (up . left) (t nil))))

(add-hook 'rcirc-mode-hook 'dim:rcirc-set-indicate-buffer-boundaries)

;;
;; as there's apparently no way to distinguish between opening a query and
;; receiving a query request from rcirc-mode-hook, we just remove the hook
;; when opening a query with C-c C-q
;;
(defadvice rcirc-cmd-query
  (around dim:remove-whois-hook activate)
  (remove-hook 'rcirc-mode-hook 'dim:rcirc-whois-on-query-from-others)
  ad-do-it
  (add-hook 'rcirc-mode-hook 'dim:rcirc-whois-on-query-from-others))

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

;; Authentication
(setq rcirc-authinfo '(("localhost" bitlbee "dim" "secret")))
(add-to-list 'auth-source-protocols '(irc "irc" "6667" "6700"))

;; build rcirc-authinfo from rcirc-server-alist and authinfo
(require 'auth-source)
(defun dim:rcirc-server-alist-get-authinfo (server-alist)
  "replace :auth in rcirc-server-alist with :password \"login:password\""
  (dolist (server server-alist)
    (let* ((host  (car server))
	   (plist (cdr server))
	   (auth  (plist-get plist :auth)))
      (when auth
	(plist-put plist
		   :password
		   (format "%s:%s"
			   auth
			   (auth-source-pick-first-password
			    :host host :port "irc" :login auth))))))
  rcirc-server-alist)

(setq rcirc-server-alist
      ;; we use ZNC
      (dim:rcirc-server-alist-get-authinfo
       '(("pgsql.tapoueh.org"
	  :port "6700"
	  :auth "dim.freenode"
	  :channels ("#postgresql" "#skytools" "#postgresqlfr"
		     "#emacs" "#el-get" "#gli" "#cvf"))

	 ("tapoueh.org"
	  :port "6700"
	  :auth "dim.lo"
	  :channels ("#vieuxcons"))

	 ("localhost" :channels ("&bitlbee")))))

(when (string-match "hi-media" (get-domain-name))
  (add-to-list 'rcirc-server-alist
	       '("irc.hi-media-techno.com"
		 :channels
		 ("#hm" "#pg" "#dba"
		  "#eurovox" "#allopass" "#comtrack" "#admin"))))

(provide 'dim-rcirc)
