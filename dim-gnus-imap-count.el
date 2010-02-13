;;; dim-gnus-imap-count.el --- Dimitri Fontaine
;;
;; http://www.emacswiki.org/emacs/GnusNiftyTricks#toc2

(defun gnus-user-format-function-t (dummy)
  (case (car gnus-tmp-method)
    (nnimap
     (message gnus-tmp-qualified-group)
     (let ((count (nnimap-request-message-count
		   gnus-tmp-qualified-group gnus-tmp-news-server)))
       (if count
	   (format "%d" (car count))
	 "?")))
    (t
     gnus-tmp-number-total)))

(defun gnus-user-format-function-y (dummy)
  (case (car gnus-tmp-method)
    (nnimap
     (let ((count (nnimap-request-message-count
		   gnus-tmp-qualified-group gnus-tmp-news-server)))
       (if count
	   (format "%d" (cadr count))
	 "?")))
    (t
     gnus-tmp-number-of-unread)))

(defvar nnimap-message-count-cache-alist nil)

(defun nnimap-message-count-cache-clear nil
  (setq nnimap-message-count-cache-alist nil))

(defun nnimap-message-count-cache-get (mbox &optional server)
  (when (nnimap-possibly-change-server server)
    (cadr (assoc (concat nnimap-current-server ":" mbox)
		 nnimap-message-count-cache-alist))))

(defun nnimap-message-count-cache-set (mbox count &optional server)
  (when (nnimap-possibly-change-server server)
    (push (list (concat nnimap-current-server ":" mbox)
		count) nnimap-message-count-cache-alist))
  count)

(defun nnimap-request-message-count (mbox &optional server)
  (let ((count (or (nnimap-message-count-cache-get mbox server)
		   (and (nnimap-possibly-change-server server)
			(progn
			  (message "Requesting message count for %s..."
				   mbox)
			  (prog1
			      (imap-mailbox-status
			       mbox '(messages unseen) nnimap-server-buffer)
			    (message "Requesting message count for %s...done"
				     mbox)))))))
    (when count
      (nnimap-message-count-cache-set mbox count server))
    count))

(add-hook 'gnus-after-getting-new-news-hook 'nnimap-message-count-cache-clear)

(provide 'dim-gnus-imap-count)
