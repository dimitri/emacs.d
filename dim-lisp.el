;;; dim-lisp.el --- Dimitri Fontaine
;;
;; Emacs and Lisp is SLIME, Quicklisp is the easy way here.

(require 'cl)
(require 'info)

(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program
      (loop for p in '("/Users/dim/dev/CL/ccl/dx86cl64"
		       "/home/dfontaine/dev/CL/ccl/lx86cl64")
	    until (file-exists-p p)
	    finally return (concat p " -K utf-8")))

(loop for p in '("/Users/dim/dev/CL/dpans2texi-1.05"
		 "/Users/dim/dev/CL/cl-yacc"
		 "/Users/dim/dev/CL/asdf/doc")
      do (add-to-list 'Info-directory-list p))

(provide 'dim-lisp)
