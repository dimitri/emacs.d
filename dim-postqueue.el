;;; dim-postqueue.el --- Dimitri Fontaine
;;
;; I run a local postfix to deliver messages --- check the queue!

(require 'el-get) ; el-get-sudo-password-process-filter

(defun dim:postqueue-p-sentinel (proc change)
  "Switch to the *postqueue* buffer once the command is done"
  ;; async processing means we get there at the end
  ;; of the subprocess --- upon other state change
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (if (looking-at "Mail queue is empty")
	  (message "Mail queue is empty.")
	(postqueue-mode)
	(set-window-buffer (selected-window) (process-buffer proc))))))

(defun dim:postqueue-p ()
  "run postqueue -p and complain loudly when there's staged message"
  (interactive)
  (let* ((name  "*postqueue*")
	 (dummy (when (get-buffer name) (erase-buffer)))
	 (postq (if (file-executable-p "/sw/sbin/postqueue") 
		    "/sw/sbin/postqueue"
		  (executable-find "postqueue")))
	 (process-connection-type nil)
	 (proc  (start-process
		 name name ;; both the process and buffer name
		 (executable-find "sudo") "-S" postq "-p")))
    (set-process-filter proc 'el-get-sudo-password-process-filter)
    (set-process-sentinel proc 'dim:postqueue-p-sentinel)))

(defun dim:postqueue-f ()
  "run postqueue -f"
  (interactive)
  (let* ((name  "*postqueue*")
	 (dummy (when (get-buffer name) (erase-buffer)))
	 (postq (if (file-executable-p "/sw/sbin/postqueue") 
		    "/sw/sbin/postqueue"
		  (executable-find "postqueue")))
	 (process-connection-type nil)
	 (proc  (start-process
		 name name ;; both the process and buffer name
		 (executable-find "sudo") "-S" postq "-f")))
    (set-process-filter proc 'el-get-sudo-password-process-filter)))

(defun postqueue-p-flush ()
  (interactive)
  (dim:postqueue-f)
  (postqueue-p-refresh))

(defun postqueue-p-refresh ()
  (interactive)
  (dim:postqueue-p))

(defvar postqueue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "f") 'postqueue-p-flush)
    (define-key map (kbd "g") 'postqueue-p-refresh)
    map)
  "Keymap for postqueue -p mode")

(define-derived-mode postqueue-mode fundamental-mode "PostQueue"
  "A major mode for postqueue interaction."
  :group 'comm
  (setq buffer-undo-list t))

(provide 'dim-postqueue)
