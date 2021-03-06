;;; .gnus
;;
(require 'dim-ports)

;; (setq user-mail-address "dimitri@2ndQuadrant.fr")
;; (setq user-mail-address "dimitri.fontaine@schibsted.com")
(setq user-mail-address "dim@tapoueh.org")
(setq user-full-name "Dimitri Fontaine")
(setq message-user-fqdn "laptop.tapoueh.org")

;; No primary select method
;(setq gnus-select-method '(nnnil ""))

;; Let's have a chance to login against local IMAP services
;; (add-to-list 'auth-sources '(:source "~/.authinfo"))

;; Use this great NNTP gateway that publishes mailing lists and RSS
(setq gnus-select-method '(nntp "news.gwene.org"))

(setq gnus-secondary-select-methods
      '((nnimap "tapoueh"
		(nnimap-address "imap.fastmail.com")
		(nnimap-server-port "imaps"))

        (nnimap "citus"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "imaps")
                (nnimap-stream ssl))

        ;; (nnimap "scm"
	;; 	(nnimap-address "imap.gmail.com")
	;; 	(nnimap-server-port "imaps")
        ;;         (nnimap-stream ssl))

        ;; (nnimap "quadrant"
	;; 	(nnimap-address "mail.2ndQuadrant.fr")
	;; 	(nnimap-server-port "imaps"))

        ;; (nnimap "lbc"
	;; 	(nnimap-address "mail.scmfrance.fr")
	;; 	(nnimap-server-port "imaps"))

	;; (nnimap "hm"
	;; 	(nnimap-address "imap.hi-media-techno.com")
	;; 	(nnimap-server-port "imaps")
	;; 	(nnimap-stream ssl))

	(nntp "news.eternal-september.org")))

;; to avoid being disconnected too often
;; (require 'gnus-demon)
;; (gnus-demon-add-handler 'gnus-group-get-new-news 5 t)

(defun dim:gnus-choose-sent-folder (current-group)
  "see gnus-message-archive-group documentation"
  (cond ((or (null current-group)
             (string-match "tapoueh" current-group))
	 "nnimap+tapoueh:INBOX.Sent Messages")

        ;; ((or (null current-group)
	;;      (string-match "quadrant" current-group))
	;;  "nnimap+quadrant:Sent Messages")

	;; ((string-match "lbc" current-group)
	;;  "nnimap+lbc:Sent Messages")

        ;; ((string-match "hm" current-group)
	;;  "nnimap+hm:Sent Messages")
        ))

(setq gnus-message-archive-group 'dim:gnus-choose-sent-folder)
(setq gnus-gcc-mark-as-read t)
(setq gnus-message-replyencrypt t)

;; Always send the email to me in BCC so that I have it in the same folder
;; as the mail I'm answering to.
;;
;; From the *Group* buffer M-K runs the command gnus-group-edit-global-kill
;; has been used in order to add the following couple of lines so that any
;; mail received that I sent is already marked as read.
;;
;; (gnus-kill "From" "Dimitri Fontaine" "d")
;;
;; The following line would allow for the email never to be visible in the
;; Summary buffer, but I prefer being able to see it. (gnus-expunge "X")
;;
(setq mail-self-blind 'self-bcc)

;; The RSS groups are set level 4 (S l), the normal groups are level 3
;; M-3 g allows to skip getting RSS updates
;; this setting makes it so that subsequent g will continue skipping
(setq gnus-group-use-permanent-levels 't)

;; Disable workaround targeting old zimbra servers
;; since newer versions have been fixed
(setq nnimap-quirks nil)

;; (when-using-msmtp
;;  (setq message-send-mail-function 'message-send-mail-with-sendmail)
;;  (when-running-macosx (setq sendmail-program "/sw/bin/msmtp"))
;;  (setq message-sendmail-extra-arguments '("-a" "himedia")))

;; (setq message-send-mail-function 'message-send-mail-with-sendmail)
;; (when-running-macosx (setq sendmail-program "/sw/sbin/sendmail"))

(setq starttls-extra-arguments '("--insecure"))

(setq message-send-mail-function 'dim:message-smtpmail-send-it)

(defvar dim:smtp-relays
  '((:naquadah "mail.tapoueh.org" starttls "submission")
    (:tapoueh  "smtp.fastmail.com" starttls "submission")
    (:hi-media "smtp.hi-media-techno.com" starttls "smtp")
    ;(:2ndQ     "91.121.90.165" starttls "submission") ; ipv6 fucks me
    (:2ndQ     "smtp.2ndQuadrant.fr" starttls "submission") ; ipv6 fucks me
    (:lbc      "smtp.scmfrance.fr" starttls "submission")
    (:scm      "smtp.gmail.com" starttls "submission")
    (:citus    "smtp.gmail.com" starttls "submission")
    (:cedric   "acidenitrix.villemain.org" starttls 26))
  "Relays Hosts to use depending on From: when sending mail.")

(defvar dim:default-smtp-relay
  '("smtp.fastmail.com" starttls "submission")
  "Default relay host.")

(defvar dim:force-using-default-smtp nil
  "When not nil, for using dim:default-smtp-relay")

(defun dim:message-smtpmail-send-it ()
  "Automatically adjust the SMTP parameters to match the From header."
  (let* ((from    (message-field-value "From"))
	 (network (cond
		   ((string-match-p "tapoueh.org" from) :tapoueh)
		   ;; ((string-match-p "hi-media.com" from) :hi-media)
		   ;; not possible anymore, VPN?
		   ((string-match-p "scmfrance.fr" from) :lbc)
		   ((string-match-p "schibsted.com" from) :scm)
		   ((string-match-p "citusdata.com" from) :citus)
		   ((string-match-p "hi-media.com" from) :2ndQ)
		   ((string-match-p "2ndQuadrant.fr" from) :2ndQ))))
    ;; get connection details from dim:smtp-relays
    (destructuring-bind (smtpmail-smtp-server
			 smtpmail-stream-type
			 smtpmail-smtp-service)
	(if dim:force-using-default-smtp
	    dim:default-smtp-relay
	  (or (cdr (assoc network dim:smtp-relays)) dim:default-smtp-relay))
      (message-smtpmail-send-it))))

(setq gnus-posting-styles
      ;; The entire alist will be iterated over, from the beginning towards
      ;; the end, and each match will be applied, which means that
      ;; attributes in later styles that match override the same attributes
      ;; in earlier matching styles.
      '(("hm"
	 (address "dfontaine@hi-media.com")
	 (organization "Hi-Media")
	 (signature-file "~/.signature")
	 ;;(eval (setq message-sendmail-extra-arguments '("-a" "himedia")))
	 (user-mail-address "dfontaine@hi-media.com"))

	("quadrant"
	 (address "dimitri@2ndQuadrant.fr")
	 (organization "2ndQuadrant")
	 (signature-file "~/.signature.2nd")
	 ;;(eval (setq message-sendmail-extra-arguments '("-a" "quadrant")))
	 (user-mail-address "dimitri@2ndQuadrant.fr"))

        ("citus"
	 (address "dimitri@citusdata.com")
	 (organization "Citus Data")
	 (signature-file "~/.signature.lists")
	 (user-mail-address "dimitri@citusdata.com"))

	;; Listes PostgreSQL
	("pgsql-"
	 ;; (header "List-ID" "postgresql.org")
	 (address "dimitri@citusdata.com")
	 (organization "Citus Data")
	 (signature-file "~/.signature.lists")
	 (user-mail-address "dimitri@citusdata.com"))

	;; listes PostgreSQL sur pgfoundry
	((header "List-Id" "pgfoundry.org")
	 (signature "dim"))

	;; comp.lang.lisp
	("comp.lang.lisp"
	 (address "dim@tapoueh.org")
	 (signature "dim")
	 (user-mail-address "dim@tapoueh.org"))

        ;; Schibsted
        ("scm"
         (address "dimitri.fontaine@schibsted.com")
         (organisation "Le Bon Coin")
         (signature-file "~/.signature.lbc")
         (user-mail-address "dimitri.fontaine@schibsted.com"))

        ;; Le Bon Coin
        ("lbc"
         (address "dimitri.fontaine@scmfrance.fr")
         (organisation "Le Bon Coin")
         (signature-file "~/.signature.lbc")
         (user-mail-address "dimitri.fontaine@scmfrance.fr"))

	;; Tapoueh
	("tapoueh"
	 (address "dim@tapoueh.org")
	 (signature "dim")
	 ;;(eval (setq message-sendmail-extra-arguments '("-a" "tapoueh")))
	 (user-mail-address "dim@tapoueh.org"))))

(setq gnus-parameters
      '(("PostgreSQL\\..*"
	 (posting-style
	  (address "dimitri@citusdata.com")
	  (organization "Citus Data")
	  (signature-file "~/.signature.lists")
	  (user-mail-address "dimitri@citusdata.com")))))

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

;; arrange to be able to get back to Gnus defaults here
(unless (boundp 'default-gnus-buffer-configuration)
  (defvar default-gnus-buffer-configuration gnus-buffer-configuration))

;; and now force the default systematically, before doing anything smart
;; about it.
(setq gnus-buffer-configuration default-gnus-buffer-configuration)

;; I still have a setup in 1024x768...
(let ((srcsize (get-screen-dimensions))
      (frame-size (list (frame-pixel-width) (frame-pixel-height))))
  (if (or (< (frame-pixel-width) 1024) ; netbook setups (1020 561) or such
	      (equal '(1024 768) srcsize)
	      (equal '(2560 1440) srcsize)
	      (equal '(1050 1680) frame-size))
      (setq gnus-buffer-configuration default-gnus-buffer-configuration)

    ;; tweak the visual configuration
    (progn
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

  (when (or (equal '(1680 1050) srcsize)
	    (equal '(1680 1050) frame-size) ; beware of multiple screens
	    (equal '(1440 867) frame-size) ; beware of multiple screens
            (equal '(1364 836) frame-size) ; normal size of a single frame
	    (equal '(1440 900) srcsize)
	    (equal '(2496 1418) srcsize)) ; thanks to VirtualBox
    (gnus-add-configuration
     ;; two panes side-by-side
     '(article (horizontal 1.0
			   (article 1.0)
			   (summary 0.5 point))))

    ;; Vertical display when replying
    (gnus-add-configuration '(reply (horizontal 1.0 (message .50 point) (article 1.0))))
    (gnus-add-configuration '(reply-yank (horizontal 1.0 (message .50 point) (article 1.0))))
    (gnus-add-configuration '(forward (horizontal 1.0 (message .50 point) (article 1.0))))))

;; flyspell
;(add-hook 'message-mode-hook 'flyspell-mode)

;; choose default dictionary
;; (defun dim:pick-dictionary-from-newsgroup-name ()
;;   "Choose the default dictionary depending on the group name, if any"
;;   (when gnus-newsgroup-name
;;     (if (string-match "PostgreSQL" gnus-newsgroup-name)
;; 	(ispell-change-dictionary "english"))
;;     (ispell-change-dictionary "francais")))
;;
;; (add-hook 'message-mode-hook 'dim:pick-dictionary-from-newsgroup-name)

;; topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; in summary mode, . jumps to first unread article
(define-key gnus-summary-mode-map (kbd ".") 'gnus-summary-first-unseen-subject)

;; start offlineimap automatically on platforms where it works ok
;; (when-running-macosx
;;  (add-hook 'gnus-group-mode-hook 'offlineimap))

;; f runs the command mbsync
;; (require 'mbsync)
;; (add-hook 'mbsync-exit-hook 'gnus-group-get-new-news)
;; (define-key gnus-group-mode-map (kbd "f") 'mbsync)

;; we don't use mailq anymore, see dim:message-smtpmail-send-it
;; (require 'mailq)
;; (define-key gnus-group-mode-map (kbd "M-q") 'mailq)
;; (define-key gnus-summary-mode-map (kbd "M-q") 'mailq)

;;
;; use iswitchb like UI everywhere sensible with
;; gnus-iswitchb-completing-read
;;
;; but we've been switching to ido
;;
(setq gnus-completing-read-function 'gnus-ido-completing-read)

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

;; (setq gnus-summary-line-format
;;       (concat "%U%R %~(max-right 17)~(pad-right 17)&user-date;  "
;; 	      "%~(max-right 20)~(pad-right 20)n %B%s\n"))

(setq gnus-summary-line-format
      (concat "%U%R %~(max-right 17)~(pad-right 17)&user-date;  "
	      "%~(max-right 20)~(pad-right 20)f %B%s\n"))

;; gnus-group-line-format, cf dim-gnus-imap-count
;; "%M%S%p%P%5y:%B%(%g%)%O\n"
;; (require 'dim-gnus-imap-count)
;; (setq gnus-group-line-format "%M%S%p%P%5uy:%B%(%g%)%O\n")
(setq gnus-group-line-format "%M\%S\%p\%P\%5y:%B%(%g%)\n")

(require 'gnus-art)
(setq gnus-visible-headers
      (concat gnus-visible-headers "\\|^User-Agent:\\|^X-Mailer:"))

(setq gnus-article-update-date-headers 60)

;; pretify the summary buffer with some unicode fun
;;
;; http://www.utf8-chartable.de/unicode-utf8-table.pl
;; http://www.utf8-chartable.de/unicode-utf8-table.pl?start=9728
;;
;; ♣ ♦ ♮ ❢ ➨ ➯ ➱ ➳ ➸ ➼ ➽ ⚑ ⚐ ⟲ ↻ ↺ ⇆ ⇷ ⇸
;; ♼ ♺ ♻ ⚛ ⚡ ⚹ ⚚ ☀ ☣ ☯ ☫ ☲ ♈ ♣
;; ♻ ♽ ◈ ■ ▪ ●
;; ⊖ ⊱ ⋋ ⋲ ∝ ∢ ∿ ≺ ⊀ ⊰ ⊱ ⊁ ≻ ⋎ ⋏ ∻ ≁ ≉ ≭ ⋇ ⋔ ⋕ ⋖ ⋗ ⋪ ⋫ ⋲ ⋺
;; ◈ ■ ▪ ● ☪ ▸ ► ▶ ⟴ ⧔ ⬛ ⬣ ⬢ ⬟ ⭓
(setq gnus-summary-to-prefix "→"
      gnus-ticked-mark ?⚑
      gnus-read-mark ?∢
      gnus-del-mark ?≭
      gnus-replied-mark ?⊱
      gnus-forwarded-mark ?∝
      gnus-unread-mark ? 
      gnus-sum-thread-tree-single-indent "♣ " ; ⚙
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-false-root "⭓ "
      gnus-sum-thread-tree-root "➽ "
      gnus-sum-thread-tree-leaf-with-other "├─≻"  ; "├─►" "┣━► "  "▶" ─⇸ ⇴ ⇥ ↠ →
      gnus-sum-thread-tree-single-leaf     "└─≻"  ; "└─►" "┗━► "
      gnus-sum-thread-tree-vertical        "│"  ) ; "┆" "┋")  "│" "┆"

;; with Tango-2 Theme, adapt some colors
(when-running-debian-or-ubuntu
 (set-face-attribute 'gnus-summary-normal-ticked nil
		     :foreground "pale violet red"))

;; don't display the <hr> like bar between header and body
(setq gnus-treat-body-boundary nil)

;; allow the html-renderer to display images
(setq gnus-blocked-images nil)

;; prefer plain text over html when there's a choice
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; display GnuPG signatures etc
(setq gnus-buttonized-mime-types '("multipart/signed" "multipart/alternative"))
(setq mm-verify-option 'always)

(defun dim:browse-postgresql-commit-sha1 (sha1)
  "browse-url the given SHA1 as a PostgreSQL commit diff"
  (browse-url
   (format "http://git.postgresql.org/gitweb/?p=postgresql.git;a=commitdiff;h=%s"
	   sha1)))

;; add buttons to PostgreSQL sha1, as in
;; Commit 729205571e81b4767efc42ad7beb53663e08d1ff added ...
(add-to-list 'gnus-button-alist
	     '("[0-9a-f]\\{40\\}"	; button regexp
	       0			; button regexp group match
	       (member			; button form (boolean filter)
		gnus-newsgroup-name
		'("nnimap+quadrant:list.pgsql-bugs"
		  "nnimap+quadrant:list.pgsql-committers"
		  "nnimap+quadrant:list.pgsql-hackers"))
	       dim:browse-postgresql-commit-sha1
	       0))			; button par (group match parameter)

;;; BBDB -- auto create new entries
(bbdb-initialize 'gnus 'message)
(setq bbdb-message-headers
      '((sender     "From" "Resent-From" "Reply-To" "Sender")
        (recipients "Resent-To" "Resent-CC" "To" "CC" "BCC" "Cc")))
(setq bbdb-update-records-p 'create)
(bbdb-mua-auto-update-init 'gnus 'message)

;; back to a very small window here, if at all (still wondering
(setq bbdb-mua-pop-up t
      bbdb-mua-pop-up-window-size 2
      bbdb-pop-up-window-size 2)

;; to review someday
(setq bbdb/gnus-summary-user-format-letter nil
      bbdb/gnus-summary-in-bbdb-format-letter nil)

;; blacklist gwene newgroups (RSS as news)
(setq bbdb-ignore-message-alist '(("Newsgroups" . "gwene.org.")
				  ("Newsgroups" . "gwene.fr.")
				  ("X-Facebook" . "facebook")
				  ("Reply-To" . "@plus.google.com")
				  ("From" . "noreply")
				  ("From" . "postmaster.twitter.com")
				  ("From" . "notify@twitter.com")
				  ("From" . "viadeonews@viadeo.com")
				  ("From" . "member@linkedin.com")
				  ("From" . "hit-reply@linkedin.com")
				  ("From" . "group-digests@linkedin.com")
				  ("Reply-To" . "support-rt@2ndquadrant.com")
				  ("Reply-To" . "support-rt-comment@2ndquadrant.com")
				  ("Reply-To" . "rt-comment@support.2ndquadrant.com")
                                  ("From" . "sysadmin@2ndquadrant.com")
				  ("From" . "notifications@github.com")
				  ("From" . "support@2ndquadrant.com")
				  ("From" . "support-comment@2ndquadrant.com")
                                  ("From" . "development@2ndquadrant.com")
				  ("From" . "on-demand-consulting@2ndquadrant.com")
				  ("From" . "on-demand-consulting-comment@2ndquadrant.com")
				  ("From" . "express@airbnb.com")
				  ("From" . "auto-message@eventbrite.com")
                                  ("From" . "ne_pas_repondre@sfr.fr")
                                  ("From" . "do-not-reply@tripit.com")
                                  ("From" . "ftpmaster@ftp-master.debian.org")
                                  ("From" . "info@meetup.com")
                                  ("From" . "invitation@capitainetrain.com")
                                  ("From" . "stay-in-touch@brewster.com")
                                  ("From" . "jira_admins@scmfrance.fr")
                                  ("From" . "admins@scmfrance.fr")
                                  ("From" . "review@bon-coin.net")
                                  ("From" . "viadeonews@viadeo.com")
                                  ("From" . "welcome-notification@viadeo.com")
                                  ("From" . "avaaz@avaaz.org")
                                  ("From" . "invitations@linkedin.com")
                                  ("From" . "contact@pgconf.eu")
                                  ("From" . "@bugs.launchpad.net")
                                  ("From" . "no_reply@edenred.com")
                                  ("From" . "support@uber.com")
                                  ("From" . "commandes@amazon.fr")
                                  ("From" . "confirmation-commande@amazon.fr")
                                  ("From" . "mailer@doodle.com")
                                  ("From" . "nepasrepondre@relation-client.3suisses.fr")
                                  ("From" . "serviceclient@email.leetchi.com")
                                  ("From" . "serviceclient@communication.o2.fr")
                                  ("From" . "helpdesk@scmfrance.fr")
                                  ("From" . "no-reply@hire.lever.co")
                                  ("From" . "support@github.com")
                                  ("From" . "nepasrepondre@agencenavigo.fr")
                                  ("From" . "@docs.google.com")
                                  ("From" . "ce@scmfrance.fr")
                                  ("From" . "postgresql-3-announce@meetup.com")))

;; display attached images and resize them
(setq mm-inline-large-images 'resize
      mm-inline-large-images-proportion 0.2)
(add-to-list 'mm-attachment-override-types "image/.*")

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

;; gwene articles contain long lines, wrap them
;; not necessary anymore thanks to the newer gnus html renderer
;; (defadvice gnus-summary-scroll-up
;;   (after dim:gnus-article-word-wrap-gwene activate)
;;   (when (string-match "gwene" gnus-newsgroup-name)
;;     (gnus-article-fill-cited-article)))

;; (defadvice gnus-summary-next-unread-article
;;   (after dim:gnus-article-word-wrap-gwene activate)
;;   (when (string-match "gwene" gnus-newsgroup-name)
;;     (gnus-article-fill-cited-article)))

;;
;; cat *current-article* | git am
;;

;; Git apply
(defcustom dim:gnus-group-git-repos
  '(el-get "~/dev/emacs/el-get")
  "A plist of repositories and dir where to apply git patches")

(defun dim:gnus-group-git-read-repo ()
  "Ask use where to apply the current patch"
  (completing-read
   "Choose a repository where to apply: "
   (loop for (r p) on dim:gnus-group-git-repos by 'cddr collect (symbol-name r)) nil t))

(defun dim:gnus-group-git-am (repo)
  (interactive (list (dim:gnus-group-git-read-repo)))
  (let ((git-dir
	 (expand-file-name
	  (plist-get dim:gnus-group-git-repos (intern repo)))))
    (when git-dir
      (gnus-summary-save-in-pipe
       (format "git --git-dir=%s/.git am -s" git-dir) 'raw))))

(define-key gnus-summary-mode-map (kbd "G A") 'dim:gnus-group-git-am)


;; dired insinuate, (info "(gnus) Other modes")
;; C-c C-m C-a send mail with attachments
;; C-c C-m C-l open with mailcap
;; C-c C-m C-p print via mailcap
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;
;; Fix printing when running MacOSX, where sending PS is broken, and sending
;; PDF works seemlessly --- guess what? yeah, that's not working
;;
(when-running-macosx
 ;; run ps2pdf on spooled buffer before to send it to printer
 (defadvice ps-do-despool
   (before dim:ps-do-despool activate)
   (unless filename
     (message "Converting PS to PDF before printing")
     (with-current-buffer ps-spool-buffer
       (shell-command-on-region
	(point-min) (point-max) "ps2pdf - -" nil 'replace)))))
