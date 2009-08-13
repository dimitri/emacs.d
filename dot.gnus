;;; .gnus
;;
(setq user-mail-address "dfontaine@hi-media.com")
(setq user-full-name "Dimitri Fontaine")

(setq gnus-select-method '(nnspool ""))

(setq gnus-secondary-select-methods
      ;; Both servers are in fact localhost, trick /etc/hosts
      '((nnimap "hm.local"
		(nnimap-address "localhost"))

	(nnimap "tapoueh.local"
		(nnimap-address "localhost"))))

(defun dim:gnus-choose-sent-folder (current-group)
  "see gnus-message-archive-group documentation"
  (if (string-match "hm.local" current-group)
      "nnimap+hm.local:Sent Messages"
    "nnimap+tapoueh.local:INBOX.Sent Messages"))
  
(setq gnus-message-archive-group 'dim:gnus-choose-sent-folder)

(setq gnus-posting-styles
      '(("hm.local"
	 (address "dfontaine@hi-media.com")
	 (organization "Hi-Media")
	 (signature-file "~/.signature"))

	;; Hi-Media listes PostgreSQL
	((header "List-ID" "postgresql.org")
	 (signature "dim"))

	;; listes PostgreSQL sur pgfoundry
	((header "List-Id" "pgfoundry.org")
	 (signature "dim"))
	
	;; Tapoueh
	("tapoueh.local"
	 (address "dim@tapoueh.org")
	 (signature "dim"))))

(setq gnus-agent nil)
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
			     '(summary-carpal 4))))))

(add-hook 'gnus-select-group-hook
	  (lambda ()
	    (cond
	     ((string-match 
	       "PostgreSQL" (gnus-group-real-name gnus-newsgroup-name))
	      (ispell-change-dictionary "english"))
	     (t
	      (ispell-change-dictionary "francais")))))
