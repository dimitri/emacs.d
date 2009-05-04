;;; dim:offlineimap.el --- launch offlineimap in a M-x term
;;

(defvar dim:offlineimap-bin "/usr/bin/offlineimap"
  "Absolute pathname where to find the offlineimap command")

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
      (insert dim:offlineimap-bin)
      (term-send-input))))

(global-set-key (kbd "C-c o") 'dim:offlineimap-start)

(provide 'dim:offlineimap)
