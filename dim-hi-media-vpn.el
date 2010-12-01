;;; dim-hi-media-vpn.el --- start local VPN when at Hi-Media
;;

(require 'el-get) ; el-get-sudo-password-process-filter

(defvar dim:hi-media-vpn-start-script "~/Hi-Media/VPN/tunnel.connect.sh"
  "Path name to the script responsible for starting the VPN connections")

(defun dim:hi-media-vpn-start ()
  "Start local VPN by means of the given script"
  (interactive)
  (let* ((name  "*VPN*")
	 (default-directory (file-name-directory dim:hi-media-vpn-start-script))
	 (proc  (start-process-shell-command
		 name name ;; both the process and buffer name
		 (executable-find "sudo") 
		 "-S" (expand-file-name dim:hi-media-vpn-start-script))))
    (set-window-buffer (selected-window) name)
    (set-process-sentinel proc
			  ;; async processing means we get there at the end
			  ;; of the subprocess --- upon other state change
			  (lambda (proc change)
			    (when (eq (process-status proc) 'exit)
			      (if (not (eq 0 (process-exit-status proc)))
				  (set-window-buffer (selected-window) (process-buffer proc))
				(message "VPN started.")
				(sleep-for 4)
				(ifconfig)))))
    (set-process-filter proc 'el-get-sudo-password-process-filter)))

(global-set-key (kbd "C-c V") 'dim:hi-media-vpn-start)

(provide 'dim-hi-media-vpn)
