;;; dim-previous-message.el --- Dimitri Fontaine
;;
;; print last message
;; current-message is already lost by the time this gets called
(defun dim:previous-message (&optional n)
  "get last line of *Message* buffer"
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let* ((n     (or n 1))
	     (nth   n)
	     (p     (point))
	     (dummy (forward-line 0))
	     (bol   (point)))
	(while (> nth 0)
	  ;; on first round, skip moving when we were not on a new empty line
	  (unless (and (eq n nth) (not (eq p bol)))
	    (forward-line -1))
	  (unless (looking-at (rx (or "Mart set"
				      "Mark activated"
				      "Auto-saving"
				      "Quit"
				      "Undo")))
	    (setq nth (- nth 1)))))
	(buffer-substring (line-beginning-position) (line-end-position)))))

(defun dim:insert-previous-message (&optional nth)
  "insert last message of *Message* to current position"
  (interactive "p")
  (insert (format "%s" (dim:previous-message nth))))

(global-set-key (kbd "C-c m") 'dim:insert-previous-message)

(provide 'dim-previous-message)
