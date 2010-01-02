;;; dim-others.el --- Dimitri Fontaine
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

;; on request by cyrilb, who missed it from vim
;; no global-set-key yet, still have to think I'll use it someday...
(defun copy-char-from-prev-line ()
  "Copy char at same position on previous line, when such a line and position exists"
  (interactive)
  (let ((c)
	(p (- (point) (line-beginning-position))))
    (save-excursion
      (when (eq 0 (forward-line -1))
	(when (< (+ (point) p) (line-end-position))
	  (forward-char p)
	  (setq c (thing-at-point 'char)))))
    (when c
      (insert c))))
