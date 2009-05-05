;;; dim:offlineimap.el --- launch offlineimap in a M-x term
;;

(defvar dim:offlineimap-bin "/usr/bin/offlineimap"
  "Absolute pathname where to find the offlineimap command")

(defvar dim:offlineimap-pause "120"
  "Delay in seconds to sleep between offlineimap invocations ")

(defun dim:offlineimap-start ()
  "Opens a M-x term and type in offlineimap"
  (interactive) 
  (let*
      ;; run-command is the term name without the "*"
      ;;
      ;; buffer-name is the term name given by ansi-term when given
      ;; run-command as the name
      ((run-command "offlineimap")
       (buffer-name (concat "*" run-command "*")))

    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      
      (ansi-term "/bin/bash" run-command)
      (set-buffer (get-buffer buffer-name))
      (insert (concat "while :; " 
		      "do " dim:offlineimap-bin "; "
		      "if [ $? -eq 0 ]; then "
		      "echo; "
		      "date; "
		      "echo sleeping " dim:offlineimap-pause " seconds; "
		      "sleep " dim:offlineimap-pause "; "
		      "fi; "
		      "done"))
      (term-send-input))))

(global-set-key (kbd "C-c o") 'dim:offlineimap-start)

(provide 'dim-offlineimap)
