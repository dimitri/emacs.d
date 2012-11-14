;;; dim-lisp.el --- Dimitri Fontaine
;;
;; Emacs and Lisp is SLIME, Quicklisp is the easy way here.

(require 'cl)
(require 'info)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq slime-net-coding-system 'utf-8-unix)

(setq inferior-lisp-program
      (loop for (path opts) in
	    '(("/Users/dim/dev/CL/ccl/dx86cl64" " -K utf-8")
	      ("/home/dfontaine/dev/CL/ccl/lx86cl64" " -K utf-8")
	      ("/usr/bin/sbcl"))
	    until (file-exists-p path)
	    finally return (if opts (concat path opts) path)))

(loop for p in '("~/dev/CL/dpans2texi-1.05"
		 "~/dev/CL/cl-yacc"
		 "~/dev/CL/asdf/doc")
      do (add-to-list 'Info-directory-list p))

(defun slime-new-repl (&optional new-port)
  "Create additional REPL for the current Lisp connection."
  (interactive)
  (if (slime-current-connection)
      (let ((port (or new-port (slime-connection-port (slime-connection)))))
        (slime-eval `(swank:create-server :port ,port))
        (slime-connect slime-lisp-host port))
    (error "Not connected")))

(provide 'dim-lisp)
