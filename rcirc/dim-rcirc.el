;;; dim setup for rcirc
;; http://www.emacswiki.org/emacs/rcirc

(require 'rcirc)
(require 'rcirc-groups)
;(require 'rcirc-notify-mode)

(add-hook 'rcirc-mode-hook (lambda () (use-hard-newlines 1)))
(add-hook 'rcirc-mode-hook 'rcirc-omit-mode)

;; add a key for EMMS integration
(defun dim:insert-current-emms-track ()
  (interactive)
  (insert (emms-track-description (emms-playlist-current-selected-track))))

(define-key rcirc-mode-map (kbd "C-c C-e") 'dim:insert-current-emms-track)

;; Exclude rcirc properties when yanking, in order to be able to send mails
;; for example.
(add-to-list 'yank-excluded-properties 'rcirc-text)

;; Some utility functions to have automated layout upon startup
(require 'cl)

(defun dim:chan-buffer (channel)
  "Return the buffer for given channel, whatever the name of the server"
  (loop for b being the buffers
	when (string-match-p (format "^%s@" channel) (buffer-name b))
	return b))

(defun dim:wait-for-buffers (buffer-list &optional n)
  "return only when all buffers in the list exist, or we tried n times"
  (let ((n (or n 2))
	(buffers))
    (while (and (< (length buffers) (length buffer-list)) (not (eq n 0)))
      (dolist (buffer-name buffer-list)
	(when (not (memq buffer-name buffers))
	  (when buffer-name
            (get-buffer buffer-name)
	    (setq buffers (cons buffer-name buffers)))))
      (message "%s, waiting for %d buffers still: %S"
	       n (- (length buffer-list) (length buffers))
	       (set-difference buffer-list buffers))
      (sleep-for 0.5)
      (decf n))
    ;; we return buffers
    buffers))

(defun dim:rcirc-layout-home-waiting ()
  "Organize screen layout for seeing the IRC connections"
  (delete-other-windows)
  (set-window-buffer (selected-window) "*tapoueh.org*")
  (set-window-buffer (split-window-horizontally) "*pgsql.tapoueh.org*"))

(defun dim:rcirc-layout-home-large ()
  "Organize screen layout for a large frame (2 columns)"
  (let* ((upper-left-window (selected-window))
	 (upper-right-window (split-window-horizontally))
	 (lower-right-window (with-selected-window upper-right-window
			       (split-window-below))))
    (set-window-buffer (selected-window) (dim:chan-buffer "#vieuxcons"))
    (set-window-buffer upper-right-window (dim:chan-buffer "#lisp"))
    (set-window-buffer lower-right-window (dim:chan-buffer "#postgresql"))
    (select-window (split-window-vertically))
    (set-window-buffer (selected-window) (dim:chan-buffer "#postgresqlfr"))
    (select-window upper-left-window)
    (split-window-vertically-quarter-bottom)
    (rcirc-groups:switch-to-groups-buffer)))

(defun dim:rcirc-layout-home-thin ()
  "Organize screen layout for a thin frame (1 useful column)"
  (let ((upper-left-window (selected-window))
	(right-window (split-window-horizontally 86)))
    (set-window-buffer (selected-window) (dim:chan-buffer "#postgresql"))
    (split-window-vertically)
    (split-window-vertically)
    (windmove-down)
    (set-window-buffer (selected-window) (dim:chan-buffer "#postgresql-apt"))
    (windmove-down)
    (split-window-vertically)
    (set-window-buffer (selected-window) (dim:chan-buffer "#vieuxcons"))
    (windmove-down)
    (set-window-buffer (selected-window) (dim:chan-buffer "#postgresqlfr"))))

(defun dim:rcirc-layout-home ()
  "Organise screen layout for IRC setup"
  (delete-other-windows)
  (let ((width    (window-width (selected-window)))
	(buffers  (dim:wait-for-buffers
		   (list
		    (dim:chan-buffer "#vieuxcons")
		    (dim:chan-buffer "#lisp")
		    (dim:chan-buffer "#postgresqlfr")
		    (dim:chan-buffer "#postgresql")))))
    (if (not (eq 4 (length buffers)))
	(dim:rcirc-layout-home-waiting)
      (if (> width (* 2 80))
	  (dim:rcirc-layout-home-large)
	(dim:rcirc-layout-home-thin)))))

(defun dim:rcirc-layout-work ()
  "Organise screen layout for IRC setup"
  (let ((buffers (dim:wait-for-buffers
		  (list
		   (dim:chan-buffer "#vieuxcons")
		   (dim:chan-buffer "#dba")
		   (dim:chan-buffer "#postgresql")
		   (dim:chan-buffer "#postgresqlfr")))))
    (if (not (eq 4 (length buffers)))
	;; yes, at work too, same servers
	(dim:rcirc-layout-home-waiting)
      (delete-other-windows)
      (let* ((right-window (split-window-horizontally))
	     (bottom-window (split-window-vertically)))
	(set-window-buffer (selected-window) (dim:chan-buffer "#postgresql"))
	(set-window-buffer bottom-window (dim:chan-buffer "#vieuxcons"))
	(select-window bottom-window)
	(set-window-buffer (split-window-vertically) (dim:chan-buffer "#postgresqlfr"))
	(set-window-buffer right-window (dim:chan-buffer "#dba"))
	(balance-windows)
	;; now cut the right part
	(select-window right-window)
	(split-window-vertically-quarter-bottom)
	(rcirc-groups:switch-to-groups-buffer)))))

;; Central place to handle connecting
(defun dim-rcirc-start (&optional kill-buffers)
  "Start rcirc and connects to default places"
  (interactive "p")

  ;; when asked via C-u, kill any existing buffer
  (when (eq kill-buffers 4)
    (loop for b being the buffers
	  when (with-current-buffer b (eq major-mode 'rcirc-mode))
	  do (kill-buffer b)))

  ;; now start rcirc
  ;; (rcirc nil)
  (dim:rcirc)

  ;; and organize a nice layout
  (if (or (string-match "apple-darwin" system-configuration)
	  (string-match "darkstar" system-name))
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
;; (defadvice rcirc-handler-PRIVMSG
;;   (before dim:rcirc-handler-PRIVMSG activate)
;;   (unless (rcirc-channel-p (car args))
;;     (rcirc-cmd-whois sender process)))

(defun rcirc-handler-generic-whois (command process sender args text)
  "generic rcirc handler for WHOIS related commands"
  (let ((nick (cadr args))
	(mesg (mapconcat 'identity (cddr args) " ")))
    (with-current-buffer (rcirc-get-buffer-create process nick)
      (rcirc-print process sender command nick mesg))))

(defun rcirc-install-whois-handlers ()
  "Install rcirc-handler-XXX for WHOIS related protocol messages"
  (dolist (cmd '(311 312 313 317 318 319 330))
    (let ((name (intern (format "rcirc-handler-%d" cmd))))
      (fset name `(lambda (process sender args text)
		   (rcirc-handler-generic-whois
		    ,(number-to-string cmd) process sender args text))))))

;; ignore some loosy messages
(defun rcirc-handler-320 (process sender args text)
  "*** 320 dim is an identified user")

(rcirc-install-whois-handlers)

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
;; (defadvice rcirc-cmd-query
;;   (around dim:remove-whois-hook activate)
;;   (remove-hook 'rcirc-mode-hook 'dim:rcirc-whois-on-query-from-others)
;;   ad-do-it
;;   (add-hook 'rcirc-mode-hook 'dim:rcirc-whois-on-query-from-others))

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

;; truncate large buffer
(setq rcirc-buffer-maximum-lines 1500)

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
(setq rcirc-authinfo '(("localhost" bitlbee "dim" "dim")))
(add-to-list 'auth-source-protocols '(irc "irc" "6667" "6700"))

;; allow using the same server more than once
(require 'auth-source)

(defun dim:auth-source-fetch-password (server)
  "Given a server with at least :host :port :login, return the :password"
  (destructuring-bind (&key host auth &allow-other-keys)
      (cdr server)
    (destructuring-bind (&key secret &allow-other-keys)
	(car (auth-source-search :host host
				 :port "irc"
				 :user auth
				 :require '(:user :secret)))
      (if (functionp secret) (funcall secret) secret))))

;; build rcirc-authinfo from rcirc-server-alist and authinfo
(defun dim:rcirc-server-alist-get-authinfo (server-alist)
  "replace :auth in rcirc-server-alist with :password \"login:password\""
  (dolist (server server-alist server-alist)
    (let* ((host  (car server))
	   (plist (cdr server))
	   (auth  (plist-get plist :auth))
	   (pass  (dim:auth-source-fetch-password server)))
      (when auth
	(plist-put plist
		   :password (format "%s:%s" auth pass))))))

;; rcirc does not know how to connect to the same server more than once, so
;; we build our own connection routine from our own rcirc-server-alist,
;; using :host rather than the server name for connecting.
(defun dim:rcirc ()
  "Connect to rcirc-server-alist servers."
  (loop
   for s in rcirc-server-alist
   collect
   (destructuring-bind (&key host
			     (port rcirc-default-port)
			     (nick rcirc-default-nick)
			     (user-name rcirc-default-user-name)
			     (full-name rcirc-default-full-name)
			     channels
			     password
			     encryption
			     &allow-other-keys
			     &aux contact (server (car s)))
       (cdr s)
     (let ((host (or host server))	; catter with server without :host
	   (connected
	    (loop for p in (rcirc-process-list)
		  thereis (string= server (process-get p :rcirc-server)))))
       (unless connected
	 (let ((process
		(rcirc-connect host port nick user-name
			       full-name channels password encryption)))
	   (process-put process :rcirc-server server)))))))

(setq rcirc-server-alist
      ;; we use ZNC
      (dim:rcirc-server-alist-get-authinfo
       '(("freenode"
	  :host "orion.naquadah.org"
	  :port "6700"
	  :auth "dim.freenode"
	  :channels ("#lisp" "#postgresql" "#postgresqlfr"
		     "#postgresql-apt" "#skytools" "#emacs"
		     "#el-get" "#gli" "#cvf"))

	 ("lost-oasis"
	  :host "orion.naquadah.org"
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
