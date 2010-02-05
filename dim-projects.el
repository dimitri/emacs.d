;;; dim-projects.el --- Dimitri Fontaine
;; load the right projects definition file

(defun get-domain-name (&optional from)
  "Returns the system domain name.  If FROM is 'resolv or nil,
returns the value defined in /etc/resolv.conf."
  (or
   (when (or (null from) (eq from 'resolv))
     (when (file-readable-p "/etc/resolv.conf")
       (with-temp-buffer
	 (insert-file-contents-literally "/etc/resolv.conf")
	 (when (re-search-forward "^domain \\([^ ]+\\)$" nil t)
	   (match-string 1))))))
  "")

(if (string-match "hi-media-techno" (get-domain-name))
    (require 'dim-projects-hm)
  (require 'dim-projects-home))

(provide 'dim-projects)
