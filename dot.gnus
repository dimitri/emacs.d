;;; .gnus
;;
(require 'dim-ports)

(setq user-mail-address "dfontaine@hi-media.com")
(setq user-full-name "Dimitri Fontaine")

;; No primary select method
(setq gnus-select-method '(nnnil ""))

(setq gnus-secondary-select-methods
      ;; Both servers are in fact localhost, trick /etc/hosts
      '((nnimap "hm.local"
		(nnimap-address "localhost"))

	(nnimap "tapoueh.local"
		(nnimap-address "localhost"))

	(nnimap "quadrant.local"
		(nnimap-address "localhost"))))

(defun dim:gnus-choose-sent-folder (current-group)
  "see gnus-message-archive-group documentation"
  (cond ((string-match "hm.local" current-group)
	 "nnimap+hm.local:Sent Messages")

	((string-match "tapoueh.local" current-group)
	 "nnimap+tapoueh.local:INBOX.Sent Messages")

	((string-match "quadrant.local" current-group)
	 "nnimap+quadrant.local:Sent Messages")))
  
(setq gnus-message-archive-group 'dim:gnus-choose-sent-folder)
(setq gnus-gcc-mark-as-read t)

;; The RSS groups are set level 4 (S l), the normal groups are level 3
;; M-3 g allows to skip getting RSS updates
;; this setting makes it so that subsequent g will continue skipping
(setq gnus-group-use-permanent-levels 't)

(when-using-msmtp
 (setq message-send-mail-function 'message-send-mail-with-sendmail)
 (when-running-macosx (setq sendmail-program "/sw/bin/msmtp"))
 (setq message-sendmail-extra-arguments '("-a" "himedia")))

(setq gnus-posting-styles
      '(("hm.local"
	 (address "dfontaine@hi-media.com")
	 (organization "Hi-Media")
	 (signature-file "~/.signature")
	 ;;(eval (setq message-sendmail-extra-arguments '("-a" "himedia")))
	 (user-mail-address "dfontaine@hi-media.com"))

	;; Hi-Media listes PostgreSQL
	((header "List-ID" "postgresql.org")
	 (signature "dim"))

	;; listes PostgreSQL sur pgfoundry
	((header "List-Id" "pgfoundry.org")
	 (signature "dim"))
	
	;; Tapoueh
	("tapoueh.local"
	 (address "dim@tapoueh.org")
	 (signature "dim")
	 ;;(eval (setq message-sendmail-extra-arguments '("-a" "tapoueh")))
	 (user-mail-address "dim@tapoueh.org"))

	("quadrant.local"
	 (address "dimitri@2ndQuadrant.fr")
	 (organization "2ndQuadrant")
	 (signature-file "~/.signature.2nd")
	 ;;(eval (setq message-sendmail-extra-arguments '("-a" "quadrant")))
	 (user-mail-address "dimitri@2ndQuadrant.fr"))))

;; fix gnus-posting-styles when we're using msmtp to add the -a account option
(when-using-msmtp
 (setq gnus-posting-styles
       (mapcar 
	(lambda (x) 
	  (cond ((and (stringp (car x)) 
		      (string= (car x) "hm.local"))
		 (append x '((eval 
			      (setq message-sendmail-extra-arguments 
				    '("-a" "himedia"))))))
		
		((and (stringp (car x)) 
		      (string= (car x) "tapoueh.local"))
		 (append x '((eval 
			      (setq message-sendmail-extra-arguments 
				    '("-a" "tapoueh"))))))

		((and (stringp (car x)) 
		      (string= (car x) "quadrant.local"))
		 (append x '((eval 
			      (setq message-sendmail-extra-arguments 
				    '("-a" "quadrant"))))))
		
		(t x)))
	gnus-posting-styles)))

;; aliases --- allow usage of TAB to expand a complete alias into an address
(add-hook 'mail-mode-hook 'mail-abbrevs-setup)
(add-hook 'message-mode-hook 'mail-abbrevs-setup)

(setq gnus-agent nil)

;; I still have a setup in 1024x768...
(when-running-debian
 (unless (equal '(1024 768) (get-screen-dimensions))
   (gnus-add-configuration
    '(article
      (vertical 1.0
		(horizontal 8
			    (group 50)
			    (summary 1.0 point) )
		(horizontal 1.0
			    (article 1.0)))))
   
   (gnus-add-configuration
    '(summary
      (vertical 1.0
		(horizontal 1.0
			    (group 50)
			    (summary 1.0 point) 
			    (if gnus-carpal
				'(summary-carpal 4))))))))

;; flyspell
(add-hook 'message-mode-hook 'flyspell-mode)

(add-hook 'message-mode-hook
	  (lambda ()
	    (cond
	     ((string-match 
	       "PostgreSQL" gnus-newsgroup-name)
	      (ispell-change-dictionary "english"))
	     (t
	      (ispell-change-dictionary "francais")))))

;; topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
	      
;;
;; gnus porn
;;
(setq gnus-user-date-format-alist
          '(((gnus-seconds-today) . "Today, %H:%M")
            ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
            (604800 . "%A %H:%M") ;;that's one week
            ((gnus-seconds-month) . "%A %d")
            ((gnus-seconds-year) . "%B %d")
            (t . "%B %d '%y"))) ;;this one is used when no other does match

(setq gnus-summary-line-format
      (concat "%U%R %~(max-right 17)~(pad-right 17)&user-date;  "
	      "%~(max-right 20)~(pad-right 20)n %B%s\n"))

;; gnus-group-line-format, cf dim-gnus-imap-count
;; "%M%S%p%P%5y:%B%(%g%)%O\n"
(require 'dim-gnus-imap-count)
(setq gnus-group-line-format "%M%S%p%P%5uy:%B%(%g%)%O\n")

(require 'gnus-art)
(setq gnus-visible-headers 
      (concat gnus-visible-headers "\\|^User-Agent:\\|^X-Mailer:"))


(setq gnus-sum-thread-tree-false-root " ♽ "
      gnus-sum-thread-tree-single-indent "⚙ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "⚈ "
      gnus-sum-thread-tree-leaf-with-other "├─►"  ; "┣━► "  "▶"
      gnus-sum-thread-tree-single-leaf     "└─►"  ; "┗━► "
      gnus-sum-thread-tree-vertical        "┆"  ) ; "┆" "┋")  "│" "┆"

;; BBDB
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/news-auto-create-p t)

;; tweak mm-attachment-override-types in nnimap+hm.local:cron.pgfouine group
(defcustom dim:mm-attachment-override-types '("text/plain")
  "mm-inline-attachment-types value to apply to dim:gnus-inline-override-groups"
  :type '(repeat regexp))

(defcustom dim:gnus-attachment-override-groups '("nnimap+hm.local:cron.pgfouine")
  "List of gnus groups where to apply dim:mm-attachment-override-types"
  :type '(repeat string))

(setq dim:mm-attachment-override-types-orig mm-attachment-override-types)

(defun dim:gnus-attachment-override-types ()
  "Apply the local override-types settings"
  ;; reset orig settings
  (setq mm-attachment-override-types dim:mm-attachment-override-types-orig)
  (when (member gnus-newsgroup-name dim:gnus-attachment-override-groups)
    (setq 
     mm-attachment-override-types 
     `(,@mm-attachment-override-types ,@dim:mm-attachment-override-types))
    (message "dim:gnus-attachment-override-types %S" 
	     mm-attachment-override-types)))

(add-hook 'gnus-select-group-hook 'dim:gnus-attachment-override-types)
