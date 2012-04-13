;;; init-slime.el --- Dimitri Fontaine
;;
;; Slime setting, mainly

(loop for p in '("/Users/dim/dev/CL/dpans2texi-1.05"
		 "/Users/dim/dev/CL/cl-yacc"
		 "/Users/dim/dev/CL/asdf/doc")
      do (add-to-list 'Info-directory-list p))

(setq inferior-lisp-program "/Users/dim/dev/CL/ccl/dx86cl64 -K utf-8")
(setq slime-net-coding-system 'utf-8-unix)

;; (setq slime-lisp-implementations
;;       '((ecl ("ecl"))
;;         (clisp ("clisp") :coding-system utf-8-unix)
;; 	(cmucl ("/Users/dim/dev/CL/cmucl/bin/lisp"))
;; 	(ccl   ("/Users/dim/dev/CL/ccl/dx86cl64"))
;; 	(sbcl ("sbcl"))))

(require 'slime)
;; (slime-setup '(inferior-slime slime-repl))
(slime-setup '(inferior-slime slime-fancy slime-repl))
