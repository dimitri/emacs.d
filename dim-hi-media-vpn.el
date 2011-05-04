;;; dim-hi-media-vpn.el --- start local VPN when at Hi-Media
;;

(require 'el-get) ; el-get-sudo-password-process-filter

(defvar dim:hi-media-vpn-start-script "~/Hi-Media/VPN/tunnel.connect.sh"
  "Path name to the script responsible for starting the VPN connections")

(defvar dim:hi-media-vpn-start-fg-script "~/Hi-Media/VPN/tunnel.connect.fg.sh"
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

(defun dim:hi-media-vpn-start-hp ()
  "Start a foreground special local VPN"
  (interactive)
  (let* ((name "OpenVPN HP")
	 (fullname (concat "*" name "*"))
	 (buffer   (get-buffer fullname))
	 (default-directory
	   (file-name-directory dim:hi-media-vpn-start-fg-script)))

    (if buffer
	(switch-to-buffer fullname)

      (ansi-term "/bin/bash" name)
      (with-current-buffer fullname
	(insert (format "sudo %s" dim:hi-media-vpn-start-fg-script))
	(term-send-input)))))

(global-set-key (kbd "C-c V P") 'dim:hi-media-vpn-start)
(global-set-key (kbd "C-c V H") 'dim:hi-media-vpn-start-hp)

(provide 'dim-hi-media-vpn)
