;;; dim-lisp.el --- Dimitri Fontaine
;;
;; Emacs and Lisp is SLIME, Quicklisp is the easy way here.

(require 'cl)
(require 'info)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq slime-net-coding-system 'utf-8-unix)

;; http://common-lisp.net/project/slime/doc/html/Multiple-Lisps.html
;; (setq slime-lisp-implementations
;;       '((cmucl ("cmucl" "-quiet"))
;;         (sbcl ("/opt/sbcl/bin/sbcl") :coding-system utf-8-unix)))
;; (NAME (PROGRAM PROGRAM-ARGS...) &key CODING-SYSTEM INIT INIT-FUNCTION ENV)

(let* ((sbcl (loop for path in '("/usr/local/bin/sbcl" "/usr/bin/sbcl")
		   until (file-exists-p path)
		   finally return path))
       (ccl  (loop for path in '("/usr/local/bin/ccl64"
				 "/Users/dim/dev/CL/ccl/dx86cl64"
				 "/home/dfontaine/dev/CL/ccl/lx86cl64")
		   until (file-exists-p path)
		   finally return path))
       (clisp (loop for path in '("/usr/local/bin/clisp" "/usr/bin/clisp")
		   until (file-exists-p path)
		   finally return path))
       (ecl   (loop for path in '("/usr/local/bin/ecl" "/usr/bin/ecl")
                    until (file-exists-p path)
                    finally return path)))

  (setq slime-lisp-implementations
	`((ccl  (,ccl "-K" "utf-8") :coding-system utf-8-unix)
	  (sbcl (,sbcl) :coding-system utf-8-unix)
	  (clisp (,clisp) :coding-system utf-8-unix)
          (ecl (,ecl) :coding-system utf-8-unix)
          (abcl ("/usr/bin/java"
                 "-cp"
                 "/usr/local/Cellar/abcl/1.5.0/libexec/abcl.jar:$CLASSPATH"
                 "org.armedbear.lisp.Main") :coding-system utf-8-unix)))

  ;; the default M-x slime is CCL still
  (setq inferior-lisp-program (concat ccl " -K utf-8")))

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

;;
;; Fix some SLIME indentation shortcomings.
;;
(put 'handling-pgsql-notices 'common-lisp-indent-function
     (get 'unwind-protect 'common-lisp-indent-function))
(put 'task-handler-bind 'common-lisp-indent-function
     (get 'let 'common-lisp-indent-function))
(put 'bind 'common-lisp-indent-function
     (get 'let 'common-lisp-indent-function))
(put 'register-groups-bind 'common-lisp-indent-function 2)
(put 'with-prefixed-accessors 'common-lisp-indent-function 2)
(put 'with-pgsql-connection 'common-lisp-indent-function 1)
(put 'with-stats-collection 'common-lisp-indent-function 1)

;;
;; Fix some weird bug
;;
(eldoc-add-command 'slime-space)

;;
;; https://www.common-lisp.net/project/slime/doc/html/slime_002dselector.html#slime_002dselector
;;
(global-set-key (kbd "C-c s") 'slime-selector)


(provide 'dim-lisp)
