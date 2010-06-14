;;; dim-ack.el --- Dimitri Fontaine
;;
;; http://stackoverflow.com/questions/2322389/ack-does-not-work-when-run-from-grep-find-in-emacs-on-windows

(defcustom ack-command (or (executable-find "ack")
			   (executable-find "ack-grep"))
  "Command to use to call ack, e.g. ack-grep under debian"
  :type 'file)

(defvar ack-command-line (concat ack-command " --nogroup --nocolor "))
(defvar ack-history nil)
(defvar ack-host-defaults-alist nil)

(defun ack ()
  "Like grep, but using ack-command as the default"
  (interactive)
  ; Make sure grep has been initialized
  (if (>= emacs-major-version 22)
      (require 'grep)
    (require 'compile))
  ; Close STDIN to keep ack from going into filter mode
  (let ((null-device (format "< %s" null-device))
        (grep-command ack-command-line)
        (grep-history ack-history)
        (grep-host-defaults-alist ack-host-defaults-alist))
    (call-interactively 'grep)
    (setq ack-history             grep-history
          ack-host-defaults-alist grep-host-defaults-alist)))

(provide 'dim-ack)
