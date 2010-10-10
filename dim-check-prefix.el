;;; dim-check-prefix.el --- check for a telco prefix in a remote database
;;

(defvar dim:check-prefix-remote-host "bdd.eurovox"
  "name of the server where to run the SQL command")

(defvar dim:check-prefix-psql-options "-U eurovox eurovox_db"
  "psql arguments to use on the remote server")

(defvar dim:check-prefix-sql
  "select c.prefix, p.nom as pays, o.code, o.nom as operateur
     from checknum('%s') c
          join pays p on p.id = c.pays 
          join operateur o on o.id = c.operateur;"
  "SQL command to get the result")

(defun dim:check-prefix (&optional dont-complete)
  "Check for given numtel against the prefix database"
  (interactive)
  (message "%S" current-prefix-arg)
  (let* ((num (read-from-minibuffer "Phone number: "))
	 (numtel (if current-prefix-arg num
		   (replace-regexp-in-string " " "0" (format "%-10s" num))))
	 (buffer-name "*check-prefix*"))
    (if (get-buffer buffer-name)
	(switch-to-buffer buffer-name)
      (generate-new-buffer buffer-name))

    (set-window-buffer (selected-window) buffer-name)

    (let ((command (format "echo \"%s\" | ssh %s psql %s" 
			   (format dim:check-prefix-sql numtel)
			   dim:check-prefix-remote-host
			   dim:check-prefix-psql-options)))
      (message command)
      (with-current-buffer buffer-name
	(save-excursion
	  (goto-char (point-max))
	  (insert "\n" (format dim:check-prefix-sql numtel) "\n")
	  (insert (shell-command-to-string command)))))))

(global-set-key (kbd "C-c P") 'dim:check-prefix)
    

(provide 'dim-check-prefix)