;;; dim-previous-message.el --- Dimitri Fontaine
;;
;; print last message
;; current-message is already lost by the time this gets called
(defun dim:previous-message (&optional nth)
  "get last line of *Message* buffer"
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (setq nth (if nth nth 1))
      (while (> nth 0)
	(previous-line)
	(setq nth (- nth 1)))
      (buffer-substring (line-beginning-position) (line-end-position)))))

(defun dim:insert-previous-message (&optional nth)
  "insert last message of *Message* to current position"
  (interactive "p")
  (insert (format "%s" (dim:previous-message nth))))

(global-set-key (kbd "C-c m") 'dim:insert-previous-message)

(provide 'dim-previous-message)
