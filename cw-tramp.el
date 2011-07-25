;;; Setting tramp for remote sudo support
;;
;; https://github.com/renard/emacs-el/blob/master/cw/cw-local.el

(eval-after-load 'tramp
  '(progn

     (defadvice tramp-error
       (around cw:tramp-error activate)
       "Allow to use sudo on a remote host:
/sudo:x@y:z ==> /multi:sshx:y:sudo:z@y:z

Based on TWB hack (http://paste.lisp.org/display/90780)."
       ;;(message (format "TRAMP-ERROR(%s %s)" vec-or-proc signal))
       (if (and (eq 'file-error signal)
		(string= "sudo" (tramp-file-name-method vec-or-proc))
		(boundp 'target-alist))
	   (progn
	     ;;(message (format "target-alist: %s" target-alist))
	     (setq target-alist
		   (cons (vector "sshx" ""
				 (tramp-file-name-host vec-or-proc)
				 "")
			 (list (vector (tramp-file-name-method vec-or-proc)
				       (unless (string= "root" (tramp-file-name-user vec-or-proc))
					 (tramp-file-name-user vec-or-proc))
				       (tramp-file-name-host vec-or-proc)
				       (tramp-file-name-localname vec-or-proc))))))
	 ad-do-it))))

(eval-after-load 'tramp-sh
  '(progn
     ;; Reload `tramp-compute-multi-hops' to make `cw:tramp-error' advice
     ;; work. WHY ????"
     (find-library "tramp-sh")
     (find-function 'tramp-compute-multi-hops)
     (forward-sexp)
     (eval-last-sexp nil)
     (kill-buffer "tramp-sh.el.gz")

     (defadvice tramp-open-connection-setup-interactive-shell
       (before cw:tramp-open-connection-setup-interactive-shell activate)
       "Add process-sentinel to tramp-shells. Kill buffer when process died."
       (set-process-sentinel
	;; Arg 0 is proc
	(ad-get-arg 0)
	(lambda (proc change)
	  (when (eq (process-status proc) 'exit)
	    (kill-buffer (process-buffer proc))))))))

(provide 'cw-tramp)

