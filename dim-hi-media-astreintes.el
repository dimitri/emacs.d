;;; dim-hi-media-astreintes.el --- Dimitri Fontaine
;;
;; send a mail to nagios-global@hi-media.com to switch who's on call.

(defun astreintes-hi-media ()
  "send a mail to nagios"
  (interactive)
  (let ((oncall
	 (completing-read "On call: " '("slardiere" "dfontaine" "pkuili"))))
    (call-process "/usr/bin/mail"
		  "-s" (format "dba: %s" oncall) "nagios-global@hi-media.com")))
