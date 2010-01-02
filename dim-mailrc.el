;;; dim-mailrc.el --- Dimitri Fontaine
;;
;; you have to have loaded dim-keys.el (dim:toggle-my-input-method) before

;; mails
;(global-set-key (kbd "C-c @") 'mail-abbrev-insert-alias)
(require 'message)
(define-key message-mode-map (kbd "C-'") 'mail-abbrev-complete-alias)
(add-hook 'message-mode-hook 'dim:toggle-my-input-method)

;; automate adding mail at point to ~/.mailrc
(require 'mail-parse)

(defun thing-at-point-bounds-of-email-address ()
  "return a cons of begin and end position of email address at point, including full name"
  (save-excursion
    (let* ((search-point (point))
	   (start (re-search-backward "[:,]" (line-beginning-position) 'move))
	   (dummy (goto-char search-point))
	   (end   (re-search-forward  "[:,]" (line-end-position) t)))
      (setq start (if start (+ 1 start)
		    (line-beginning-position)))
      (unless end (setq end (line-end-position)))
      (cons start end))))

(defun thing-at-point-email-address ()
  "return full email address at point"
  (let* ((bounds (thing-at-point-bounds-of-email-address))
	 (email-address-text
	  (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (mail-header-parse-address email-address-text)))

(put 'email-address 'bounds-of-thing-at-point 'thing-at-point-bounds-of-email-address)
(put 'email-address 'thing-at-point 'thing-at-point-email-address)

(defun dim:mailrc-add-entry (&optional prefix alias)
  "read email at point and add it to an ~/.mailrc file"
  (interactive "P\nMalias: ")
  (let* ((default-mailrc (file-name-nondirectory mail-personal-alias-file))
	 (mailrc (if prefix (expand-file-name
			     (read-file-name 
			      "Add alias into file: " 
			      "~/" 
			      default-mailrc
			      t
			      default-mailrc))
		   mail-personal-alias-file))
	 (address (thing-at-point 'email-address))
	 (buffer (find-file-noselect mailrc t)))
    (when address
      (with-current-buffer buffer
	;; we don't support updating existing alias in the file
	(save-excursion
	  (goto-char (point-min))
	  (if (search-forward (concat "alias " alias) nil t)
	      (error "Alias %s is already present in .mailrc" alias)))

	(save-current-buffer
	  (save-excursion
	    (goto-char (point-max))
	    (insert (format "\nalias %s \"%s <%s>\"" alias (cdr address) (car address)))))))))

(global-set-key (kbd "C-c C-@") 'dim:mailrc-add-entry)

(provide 'dim-mailrc)
