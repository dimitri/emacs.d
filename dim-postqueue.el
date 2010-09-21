;;; dim-postqueue.el --- Dimitri Fontaine
;;
;; I run a local postfix to deliver messages --- check the queue!

(defvar postqueue (if (file-executable-p "/sw/sbin/postqueue") 
		      "/sw/sbin/postqueue"
		    (executable-find "postqueue"))
  "Where to find the `postqueue' utility")

(defun postqueue-p-filter (proc string)
  "Filter for `postqueue -p`, with sudo password support"
  (unless (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      ;; arrange to remember already seen content
      (save-excursion
	;; redirect the subprocess sudo prompt to the user face
	(when (string-match "password" string)
	  (let* ((prompt (thing-at-point 'line))
		 (pass   (read-passwd prompt)))
	    (process-send-string proc (concat pass "\n"))))

	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (dolist (line (split-string string "\n"))
	    (cond ((string= "" line) t)

		  ((string-match "password" line) t)

		  ((string-match "Mail queue is empty" line) 
		   ;; the sentinel will need to find this
		   (insert line))

		  ((string-match "Queue ID" line) 
		   (insert (propertize line 'face font-lock-string-face) "\n"))

		  ((string-match "^--" line)
		   (insert (propertize line 'face font-lock-comment-face) "\n"))

		  ((string-match "^ " line)
		   (insert (propertize line 'face font-lock-string-face) "\n"))

		  (t
		   ;; first line of message, queue-id size, time, sender
		   (let* ((split  (split-string line))
			  (id     (car split))
			  (size   (cadr split))
			  (mail   (car (last split)))
			  (lpart  (car (split-string mail "@")))
			  (domain (cadr (split-string mail "@")))
			  (time   (mapconcat 
				   'identity (cddr (butlast split)) " ")))
		     (insert
		      (propertize id
				  'face font-lock-constant-face
				  'id id
				  'site domain) " "
		      (format "%8s" size) " "
		      time " "
		      (propertize lpart  
				  'face font-lock-preprocessor-face) "@"
		      (propertize domain
				  'face font-lock-keyword-face) "\n"))))))))))

(defun postqueue-p-sentinel (proc change)
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

(defun postqueue-p ()
  "run postqueue -p and complain loudly when there's staged message"
  (interactive)
  (let* ((name  "*postqueue*")
	 (dummy (when (get-buffer name)
		  (with-current-buffer name
		    (let ((inhibit-read-only t)) (erase-buffer)))))
	 (process-connection-type nil)
	 (proc  (start-process
		 name name ;; both the process and buffer name
		 (executable-find "sudo") "-S" postqueue "-p")))
    (set-process-filter proc 'postqueue-p-filter)
    (set-process-sentinel proc 'postqueue-p-sentinel)))

(defun postqueue-f-sentinel (proc change)
  "Once `postqueue -f` is done, rerun `postqueue -p`"
  (when (eq (process-status proc) 'exit)
    (kill-buffer (process-buffer proc))
    (postqueue-p)))

(defun postqueue-f ()
  "run postqueue -f"
  (interactive)
  (let* ((name  "*postqueue -f*")
	 (process-connection-type nil)
	 (proc
	  (start-process name name ;; both the process and buffer name
			 (executable-find "sudo") "-S" postqueue "-f")))
    ;; filter for sudo
    (set-process-filter proc 'postqueue-p-filter)
    (set-process-sentinel proc 'postqueue-f-sentinel)))

(defun postqueue-i (id)
  "run postqueue -i ID"
  (let* ((name  "*postqueue -i*")
	 (process-connection-type nil)
	 (proc  
	  (start-process name name ;; both the process and buffer name
			 (executable-find "sudo") "-S" postqueue "-i" id)))
    ;; filter for sudo
    (set-process-filter proc 'postqueue-p-filter)
    (set-process-sentinel proc 'postqueue-f-sentinel)))

(defun postqueue-s (site)
  "run postqueue -i ID"
  (let* ((name  "*postqueue -i*")
	 (process-connection-type nil)
	 (proc  
	  (start-process name name ;; both the process and buffer name
			 (executable-find "sudo") "-S" postqueue "-s" site)))
    ;; filter for sudo
    (set-process-filter proc 'postqueue-p-filter)
    (set-process-sentinel proc 'postqueue-f-sentinel)))

(defun postqueue-p-flush ()
  (interactive)
  (postqueue-f))

(defun postqueue-p-refresh ()
  (interactive)
  (postqueue-p))

(defun postqueue-p-next-id ()
  (interactive)
  (forward-line 1)
  (let ((next (next-single-property-change (point) 'id)))
    (if (not next)
	(forward-line -1)
      (goto-char next)
      (beginning-of-line))))

(defun postqueue-p-previous-id ()
  (interactive)
  (forward-line -1)
  (let ((previous (previous-single-property-change (point) 'id)))
    (if (not previous)
	(forward-line 1)
      (goto-char previous)
      (beginning-of-line))))

(defun postqueue-p-kill-id ()
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (when id
      (message id)
      (kill-new id))))

(defun postqueue-p-kill-site ()
  (interactive)
  (let ((site (get-text-property (point) 'site)))
    (when site
      (message site)
      (kill-new site))))

(defun postqueue-p-deliver-id ()
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (when id
      (message id)
      (postqueue-i id))))

(defun postqueue-p-deliver-site ()
  (interactive)
  (let ((site (get-text-property (point) 'site)))
    (when site
      (message site)
      (postqueue-s site))))

(defvar postqueue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "f") 'postqueue-p-flush)
    (define-key map (kbd "g") 'postqueue-p-refresh)
    (define-key map (kbd "p") 'postqueue-p-previous-id)
    (define-key map (kbd "n") 'postqueue-p-next-id)
    (define-key map (kbd "w") 'postqueue-p-kill-id)
    (define-key map (kbd "i") 'postqueue-p-kill-id)
    (define-key map (kbd "s") 'postqueue-p-kill-site)
    (define-key map (kbd "I") 'postqueue-p-deliver-id)
    (define-key map (kbd "S") 'postqueue-p-deliver-site)
    map)
  "Keymap for postqueue -p mode")

(define-derived-mode postqueue-mode fundamental-mode "PostQueue"
  "A major mode for postqueue interaction."
  :group 'comm
  (setq buffer-read-only t)
  (setq buffer-undo-list t))

(provide 'dim-postqueue)
