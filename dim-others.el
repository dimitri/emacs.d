;; no provide, not required, random functions and pastes

(defun twinkle-call-symbol-or-region ()
  "Call the phone number at point (symbol seems good enough), or in region"
  (interactive)
  (shell-command-to-string 
   (format "twinkle --cmd 'call %s'"
	   (replace-regexp-in-string 
	    "[^0-9+]" "" 
	    (if (use-region-p)
		(buffer-substring (region-beginning) (region-end))
	      (thing-at-point 'symbol))))))

