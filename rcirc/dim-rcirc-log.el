;; dim-rcirc-log.el
;;
; I want rcirc log to file to have a different timestamp format than what
; I'm seeing when connected live, namely I want to add the date.
;

(defcustom rcirc-log-time-format "%Y-%m-%d %H:%M "
  "*Describes how timestamps are printed to log file.
Used as the first arg to `format-time-string'."
  :type 'string
  :group 'rcirc)

(defun rcirc-log (process sender response target text)
  "Record line in `rcirc-log', to be later written to disk."
  (let ((filename (funcall rcirc-log-filename-function process target)))
    (unless (null filename)
      (let ((cell (assoc-string filename rcirc-log-alist))
	    (line (concat (format-time-string rcirc-log-time-format)
			  (substring-no-properties
			   (rcirc-format-response-string process sender
							 response target text))
			  "\n")))
	(if cell
	    (setcdr cell (concat (cdr cell) line))
	  (setq rcirc-log-alist
		(cons (cons filename line) rcirc-log-alist)))))))

(provide 'rcirc-custom-log)
