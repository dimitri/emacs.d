;;; dim-monip.el
;;; Use monip.org services to output current external ip address

(defun dim:monip (&optional insert)
  "M-x dim:monip"
  (interactive "P")
  (let ((html (url-retrieve-synchronously "http://monip.org")))
    (with-current-buffer html
      (goto-char (point-min))
      (re-search-forward "IP : \\([0-9.]+\\)")
      (let ((ip (match-string 1)))
	(kill-new ip)
	(if (called-interactively-p)
	    (if (eq insert 1)
		(message "Your current external IP address is: '%s'." ip)
	      (insert "%s" ip)))
	ip))))

(provide 'dim-monip)
